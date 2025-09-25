;; Decentralized Perpetual Futures Exchange
;; A protocol for leveraged perpetual futures trading on Stacks assets

;; Constants  
(define-constant contract-owner tx-sender)
(define-constant max-leverage u20) ;; 20x maximum leverage
(define-constant min-leverage u2) ;; 2x minimum leverage
(define-constant liquidation-threshold u8000) ;; 80% maintenance margin (8000/10000)
(define-constant funding-rate-cap u1000) ;; 10% max daily funding rate (1000/10000)
(define-constant trading-fee-rate u30) ;; 0.3% trading fee (30/10000)
(define-constant liquidation-fee-rate u500) ;; 5% liquidation fee (500/10000)
(define-constant price-precision u100000000) ;; 8 decimal places for prices
(define-constant size-precision u1000000) ;; 6 decimal places for position sizes
(define-constant funding-interval u144) ;; Funding every ~24 hours (144 blocks)

;; Error codes
(define-constant err-not-authorized (err u100))
(define-constant err-market-not-found (err u101))
(define-constant err-insufficient-collateral (err u102))
(define-constant err-position-not-found (err u103))
(define-constant err-invalid-leverage (err u104))
(define-constant err-liquidation-not-needed (err u105))
(define-constant err-market-closed (err u106))
(define-constant err-insufficient-liquidity (err u107))
(define-constant err-price-impact-too-high (err u108))
(define-constant err-funding-not-ready (err u109))
(define-constant err-invalid-order-size (err u110))

;; Market configuration
(define-map perpetual-markets
  { market-id: uint }
  {
    base-asset: (string-ascii 12), ;; "STX", "stSTX", "wBTC" etc
    quote-asset: (string-ascii 12), ;; Usually "USDC" 
    is-active: bool,
    max-leverage: uint,
    maintenance-margin-rate: uint,
    funding-rate: int, ;; Can be negative
    last-funding-time: uint,
    mark-price: uint,
    index-price: uint,
    total-long-positions: uint,
    total-short-positions: uint,
    total-long-size: uint,
    total-short-size: uint,
    open-interest: uint
  }
)

;; User positions
(define-map trader-positions
  { trader: principal, market-id: uint }
  {
    position-size: int, ;; Positive for long, negative for short
    entry-price: uint,
    leverage: uint,
    collateral: uint,
    unrealized-pnl: int,
    last-funding-payment: uint,
    liquidation-price: uint,
    is-active: bool
  }
)

;; Liquidity pool for each market
(define-map liquidity-pools
  { market-id: uint }
  {
    total-liquidity: uint,
    available-liquidity: uint,
    total-lp-tokens: uint,
    accumulated-fees: uint,
    utilization-rate: uint,
    lp-token-price: uint
  }
)

(define-map lp-positions
  { lp: principal, market-id: uint }
  {
    lp-tokens: uint,
    deposited-amount: uint,
    entry-price: uint,
    accumulated-rewards: uint,
    last-claim: uint
  }
)

;; Orders and trading
(define-map pending-orders
  { order-id: uint }
  {
    trader: principal,
    market-id: uint,
    order-type: (string-ascii 10), ;; "market", "limit", "stop"
    side: (string-ascii 5), ;; "long", "short"
    size: uint,
    price: uint,
    leverage: uint,
    collateral: uint,
    created-at: uint,
    expires-at: uint,
    is-filled: bool,
    is-cancelled: bool
  }
)

;; Liquidation system
(define-map liquidation-queue
  { liquidation-id: uint }
  {
    trader: principal,
    market-id: uint,
    liquidation-price: uint,
    position-size: int,
    collateral-seized: uint,
    liquidator: (optional principal),
    created-at: uint,
    is-processed: bool
  }
)

;; Risk management and oracle data
(define-map price-feeds
  { asset: (string-ascii 12) }
  {
    price: uint,
    last-update: uint,
    confidence: uint,
    is-active: bool
  }
)

;; Global state variables
(define-data-var next-market-id uint u1)
(define-data-var next-order-id uint u1)
(define-data-var next-liquidation-id uint u1)
(define-data-var total-protocol-fees uint u0)
(define-data-var insurance-fund uint u0)
(define-data-var emergency-pause bool false)
(define-data-var max-price-impact uint u500) ;; 5% max price impact

;; Helper functions
(define-private (calculate-position-value (size int) (price uint))
  (if (> size 0)
    (* (to-uint size) price)
    (* (to-uint (- size)) price))
)

(define-private (calculate-liquidation-price (entry-price uint) (leverage uint) (is-long bool))
  (let ((liquidation-buffer (/ (* entry-price u2000) u10000))) ;; 20% buffer
    (if is-long
      (- entry-price (/ liquidation-buffer leverage))
      (+ entry-price (/ liquidation-buffer leverage))))
)

(define-private (calculate-funding-payment (position-size int) (funding-rate int) (time-elapsed uint))
  (let ((size-abs (if (> position-size 0) (to-uint position-size) (to-uint (- position-size))))
        (funding-amount (/ (* size-abs (if (> funding-rate 0) (to-uint funding-rate) (to-uint (- funding-rate))) time-elapsed) u86400))) ;; Daily rate
    (if (> position-size 0)
      (if (> funding-rate 0) (to-int funding-amount) (- (to-int funding-amount)))
      (if (> funding-rate 0) (- (to-int funding-amount)) (to-int funding-amount))))
)

(define-private (calculate-trading-fee (size uint) (price uint))
  (/ (* size price trading-fee-rate) u10000)
)

;; Market Management Functions

;; 1. Create new perpetual market
(define-public (create-market (base-asset (string-ascii 12)) (quote-asset (string-ascii 12)) (initial-price uint))
  (let ((market-id (var-get next-market-id)))
    
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    
    ;; Create market configuration
    (map-set perpetual-markets { market-id: market-id }
      {
        base-asset: base-asset,
        quote-asset: quote-asset,
        is-active: true,
        max-leverage: max-leverage,
        maintenance-margin-rate: liquidation-threshold,
        funding-rate: 0,
        last-funding-time: stacks-block-height,
        mark-price: initial-price,
        index-price: initial-price,
        total-long-positions: u0,
        total-short-positions: u0,
        total-long-size: u0,
        total-short-size: u0,
        open-interest: u0
      })
    
    ;; Initialize liquidity pool
    (map-set liquidity-pools { market-id: market-id }
      {
        total-liquidity: u0,
        available-liquidity: u0,
        total-lp-tokens: u0,
        accumulated-fees: u0,
        utilization-rate: u0,
        lp-token-price: price-precision
      })
    
    (var-set next-market-id (+ market-id u1))
    
    (print {
      event: "market-created",
      market-id: market-id,
      base-asset: base-asset,
      quote-asset: quote-asset,
      initial-price: initial-price
    })
    
    (ok market-id)
  )
)

;; 2. Add liquidity to market
(define-public (add-liquidity (market-id uint) (amount uint))
  (let ((market (unwrap! (map-get? perpetual-markets { market-id: market-id }) err-market-not-found))
        (pool (unwrap! (map-get? liquidity-pools { market-id: market-id }) err-market-not-found)))
    
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (get is-active market) err-market-closed)
    
    ;; Transfer collateral from LP to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Calculate LP tokens to mint
    (let ((lp-tokens-to-mint (if (is-eq (get total-lp-tokens pool) u0)
                               amount ;; First LP gets 1:1 ratio
                               (/ (* amount (get total-lp-tokens pool)) (get total-liquidity pool))))
          (existing-lp (map-get? lp-positions { lp: tx-sender, market-id: market-id })))
      
      ;; Update or create LP position
      (match existing-lp
        position
          (map-set lp-positions { lp: tx-sender, market-id: market-id }
            (merge position {
              lp-tokens: (+ (get lp-tokens position) lp-tokens-to-mint),
              deposited-amount: (+ (get deposited-amount position) amount)
            }))
        (map-set lp-positions { lp: tx-sender, market-id: market-id }
          {
            lp-tokens: lp-tokens-to-mint,
            deposited-amount: amount,
            entry-price: (get lp-token-price pool),
            accumulated-rewards: u0,
            last-claim: stacks-block-height
          }))
      
      ;; Update pool state
      (map-set liquidity-pools { market-id: market-id }
        (merge pool {
          total-liquidity: (+ (get total-liquidity pool) amount),
          available-liquidity: (+ (get available-liquidity pool) amount),
          total-lp-tokens: (+ (get total-lp-tokens pool) lp-tokens-to-mint)
        }))
      
      (print {
        event: "liquidity-added",
        lp: tx-sender,
        market-id: market-id,
        amount: amount,
        lp-tokens: lp-tokens-to-mint
      })
      
      (ok lp-tokens-to-mint)
    )
  )
)

;; 3. Open position (market order)
(define-public (open-position (market-id uint) (side (string-ascii 5)) (size uint) (leverage uint) (collateral uint))
  (let ((market (unwrap! (map-get? perpetual-markets { market-id: market-id }) err-market-not-found))
        (pool (unwrap! (map-get? liquidity-pools { market-id: market-id }) err-market-not-found))
        (existing-position (map-get? trader-positions { trader: tx-sender, market-id: market-id })))
    
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (get is-active market) err-market-closed)
    (asserts! (and (>= leverage min-leverage) (<= leverage (get max-leverage market))) err-invalid-leverage)
    (asserts! (>= (get available-liquidity pool) size) err-insufficient-liquidity)
    
    ;; Transfer collateral from trader
    (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
    
    ;; Calculate trading fee
    (let ((trading-fee (calculate-trading-fee size (get mark-price market)))
          (is-long (is-eq side "long"))
          (liquidation-price (calculate-liquidation-price (get mark-price market) leverage is-long))
          (position-size-signed (if is-long (to-int size) (- (to-int size)))))
      
      ;; Deduct trading fee from collateral
      (let ((net-collateral (- collateral trading-fee)))
        
        ;; Update or create position
        (match existing-position
          position
            ;; Modify existing position (simplified - would need proper position modification logic)
            (map-set trader-positions { trader: tx-sender, market-id: market-id }
              (merge position {
                position-size: (+ (get position-size position) position-size-signed),
                collateral: (+ (get collateral position) net-collateral),
                liquidation-price: liquidation-price
              }))
          ;; Create new position
          (map-set trader-positions { trader: tx-sender, market-id: market-id }
            {
              position-size: position-size-signed,
              entry-price: (get mark-price market),
              leverage: leverage,
              collateral: net-collateral,
              unrealized-pnl: 0,
              last-funding-payment: stacks-block-height,
              liquidation-price: liquidation-price,
              is-active: true
            }))
        
        ;; Update market state
        (if is-long
          (map-set perpetual-markets { market-id: market-id }
            (merge market {
              total-long-positions: (+ (get total-long-positions market) u1),
              total-long-size: (+ (get total-long-size market) size),
              open-interest: (+ (get open-interest market) size)
            }))
          (map-set perpetual-markets { market-id: market-id }
            (merge market {
              total-short-positions: (+ (get total-short-positions market) u1),
              total-short-size: (+ (get total-short-size market) size),
              open-interest: (+ (get open-interest market) size)
            })))
        
        ;; Update liquidity pool utilization
        (map-set liquidity-pools { market-id: market-id }
          (merge pool {
            available-liquidity: (- (get available-liquidity pool) size),
            accumulated-fees: (+ (get accumulated-fees pool) trading-fee),
            utilization-rate: (/ (* (- (get total-liquidity pool) (- (get available-liquidity pool) size)) u10000) (get total-liquidity pool))
          }))
        
        ;; Update protocol fees
        (var-set total-protocol-fees (+ (var-get total-protocol-fees) (/ trading-fee u2))) ;; 50% to protocol
        
        (print {
          event: "position-opened",
          trader: tx-sender,
          market-id: market-id,
          side: side,
          size: size,
          price: (get mark-price market),
          leverage: leverage,
          collateral: net-collateral
        })
        
        (ok true)
      )
    )
  )
)

;; 4. Close position
(define-public (close-position (market-id uint) (size-to-close uint))
  (let ((position (unwrap! (map-get? trader-positions { trader: tx-sender, market-id: market-id }) err-position-not-found))
        (market (unwrap! (map-get? perpetual-markets { market-id: market-id }) err-market-not-found)))
    
    (asserts! (not (var-get emergency-pause)) err-not-authorized)
    (asserts! (get is-active position) err-position-not-found)
    
    (let ((position-size-abs (if (> (get position-size position) 0) 
                               (to-uint (get position-size position)) 
                               (to-uint (- (get position-size position)))))
          (is-long (> (get position-size position) 0))
          (current-price (get mark-price market)))
      
      (asserts! (<= size-to-close position-size-abs) err-invalid-order-size)
      
      ;; Calculate PnL (keep everything as uint to avoid type issues)
      (let ((entry-price (get entry-price position))
            (price-diff-abs (if is-long
                              (if (>= current-price entry-price) (- current-price entry-price) u0)
                              (if (>= entry-price current-price) (- entry-price current-price) u0)))
            (is-profit (if is-long (>= current-price entry-price) (>= entry-price current-price)))
            (pnl-abs (/ (* size-to-close price-diff-abs) entry-price))
            (trading-fee (calculate-trading-fee size-to-close current-price)))
        
        ;; Calculate collateral to return
        (let ((collateral-ratio (/ size-to-close position-size-abs))
              (collateral-to-return (/ (* (get collateral position) collateral-ratio) u1)))
          
          ;; Handle payout based on profit/loss
          (if is-profit
            ;; Profit case: return collateral + profit - fee
            (if (>= pnl-abs trading-fee)
              (let ((net-profit (- pnl-abs trading-fee))
                    (total-payout (+ collateral-to-return net-profit)))
                (try! (as-contract (stx-transfer? total-payout (as-contract tx-sender) tx-sender))))
              (let ((net-loss (- trading-fee pnl-abs))
                    (remaining-collateral (if (>= collateral-to-return net-loss)
                                             (- collateral-to-return net-loss)
                                             u0)))
                (if (> remaining-collateral u0)
                  (try! (as-contract (stx-transfer? remaining-collateral (as-contract tx-sender) tx-sender)))
                  (var-set insurance-fund (+ (var-get insurance-fund) (- net-loss collateral-to-return))))))
            ;; Loss case: handle loss + fee
            (let ((total-loss (+ pnl-abs trading-fee)))
              (if (<= total-loss collateral-to-return)
                ;; Loss is covered by collateral
                (let ((remaining-collateral (- collateral-to-return total-loss)))
                  (if (> remaining-collateral u0)
                    (try! (as-contract (stx-transfer? remaining-collateral (as-contract tx-sender) tx-sender)))
                    true))
                ;; Loss exceeds collateral - add excess to insurance fund
                (var-set insurance-fund (+ (var-get insurance-fund) (- total-loss collateral-to-return))))))
          
          ;; Update position
          (if (is-eq size-to-close position-size-abs)
            ;; Close entire position
            (map-set trader-positions { trader: tx-sender, market-id: market-id }
              (merge position { is-active: false, position-size: 0 }))
            ;; Partial close
            (let ((remaining-size (- position-size-abs size-to-close))
                  (new-position-size (if is-long (to-int remaining-size) (- (to-int remaining-size)))))
              (map-set trader-positions { trader: tx-sender, market-id: market-id }
                (merge position {
                  position-size: new-position-size,
                  collateral: (- (get collateral position) collateral-to-return)
                }))))
          
          (print {
            event: "position-closed",
            trader: tx-sender,
            market-id: market-id,
            size-closed: size-to-close,
            pnl: 0, ;; Simplified for now - complex signed arithmetic
            payout: (if is-profit
                      (if (>= pnl-abs trading-fee)
                        (+ collateral-to-return (- pnl-abs trading-fee))
                        (if (>= collateral-to-return (- trading-fee pnl-abs)) (- collateral-to-return (- trading-fee pnl-abs)) u0))
                      (let ((total-loss (+ pnl-abs trading-fee)))
                        (if (<= total-loss collateral-to-return) (- collateral-to-return total-loss) u0)))
          })
          
          (ok 0)
        )
      )
    )
  )
)

;; 5. Liquidate undercollateralized position
(define-public (liquidate-position (trader principal) (market-id uint))
  (let ((position (unwrap! (map-get? trader-positions { trader: trader, market-id: market-id }) err-position-not-found))
        (market (unwrap! (map-get? perpetual-markets { market-id: market-id }) err-market-not-found)))
    
    (asserts! (get is-active position) err-position-not-found)
    
    ;; Check if position should be liquidated
    (let ((current-price (get mark-price market))
          (is-long (> (get position-size position) 0)))
      
      (asserts! 
        (if is-long
          (<= current-price (get liquidation-price position))
          (>= current-price (get liquidation-price position)))
        err-liquidation-not-needed)
      
      ;; Process liquidation
      (let ((liquidation-id (var-get next-liquidation-id))
            (position-value (calculate-position-value (get position-size position) current-price))
            (liquidation-fee (/ (* position-value liquidation-fee-rate) u10000))
            (remaining-collateral (- (get collateral position) liquidation-fee)))
        
        ;; Create liquidation record
        (map-set liquidation-queue { liquidation-id: liquidation-id }
          {
            trader: trader,
            market-id: market-id,
            liquidation-price: current-price,
            position-size: (get position-size position),
            collateral-seized: (get collateral position),
            liquidator: (some tx-sender),
            created-at: stacks-block-height,
            is-processed: true
          })
        
        ;; Close position
        (map-set trader-positions { trader: trader, market-id: market-id }
          (merge position { 
            is-active: false, 
            position-size: 0,
            collateral: u0
          }))
        
        ;; Pay liquidator
        (try! (as-contract (stx-transfer? liquidation-fee tx-sender tx-sender)))
        
        ;; Add remaining to insurance fund
        (if (> remaining-collateral u0)
          (var-set insurance-fund (+ (var-get insurance-fund) remaining-collateral))
          true)
        
        (var-set next-liquidation-id (+ liquidation-id u1))
        
        (print {
          event: "position-liquidated",
          trader: trader,
          liquidator: tx-sender,
          market-id: market-id,
          liquidation-price: current-price
        })
        
        (ok liquidation-id)
      )
    )
  )
)

;; Read-only functions

(define-read-only (get-market (market-id uint))
  (map-get? perpetual-markets { market-id: market-id })
)

(define-read-only (get-position (trader principal) (market-id uint))
  (map-get? trader-positions { trader: trader, market-id: market-id })
)

(define-read-only (get-liquidity-pool (market-id uint))
  (map-get? liquidity-pools { market-id: market-id })
)

(define-read-only (get-lp-position (lp principal) (market-id uint))
  (map-get? lp-positions { lp: lp, market-id: market-id })
)

(define-read-only (calculate-unrealized-pnl (trader principal) (market-id uint))
  (match (map-get? trader-positions { trader: trader, market-id: market-id })
    position
      (match (map-get? perpetual-markets { market-id: market-id })
        market
          (let ((is-long (> (get position-size position) 0))
                (size-abs (if is-long (to-uint (get position-size position)) (to-uint (- (get position-size position)))))
                (entry-price (get entry-price position))
                (current-price (get mark-price market)))
            (some (if is-long
              (- current-price entry-price)
              (- entry-price current-price))))
        none)
    none)
)

(define-read-only (get-protocol-stats)
  {
    total-markets: (- (var-get next-market-id) u1),
    total-protocol-fees: (var-get total-protocol-fees),
    insurance-fund: (var-get insurance-fund),
    emergency-pause: (var-get emergency-pause)
  }
)

;; Admin functions

(define-public (update-mark-price (market-id uint) (new-price uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (match (map-get? perpetual-markets { market-id: market-id })
      market
        (begin
          (map-set perpetual-markets { market-id: market-id }
            (merge market { mark-price: new-price, index-price: new-price }))
          (ok new-price))
      err-market-not-found)
  )
)

(define-public (withdraw-protocol-fees)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (let ((fees (var-get total-protocol-fees)))
      (var-set total-protocol-fees u0)
      (try! (as-contract (stx-transfer? fees tx-sender contract-owner)))
      (ok fees)
    )
  )
)

(define-public (emergency-pause-toggle)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (var-set emergency-pause (not (var-get emergency-pause)))
    (ok (var-get emergency-pause))
  )
)