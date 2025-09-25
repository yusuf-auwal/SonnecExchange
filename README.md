# Sonnec Perpetual Futures Exchange

A decentralized perpetual futures trading platform built on Stacks, enabling leveraged trading with automated liquidation and funding rate mechanisms.

## Core Features

### Trading Parameters
- Leverage range: 2x to 20x
- Liquidation threshold: 80% maintenance margin
- Maximum funding rate: 10% daily
- Funding interval: ~24 hours (144 blocks)

### Fee Structure
- Trading fee: 0.3%
- Liquidation fee: 5%
- Insurance fund accumulation

### Precision & Technical Details
- Price precision: 8 decimal places
- Position size precision: 6 decimal places
- Block-based timing system
- Automated funding rate system

## Core Components

### Market System
- Multiple market support
- Price feed integration
- Open interest tracking
- Market state management

### Position Management
- Long/short position support
- Collateral tracking
- PnL calculation
- Liquidation price monitoring

### Liquidity System
- Liquidity pool management
- LP token minting/burning
- Fee distribution
- Utilization tracking

### Risk Management
- Automated liquidations
- Insurance fund
- Emergency pause mechanism
- Maximum price impact controls

## Trading Features

### Position Operations
- Market order execution
- Position sizing
- Leverage management
- Collateral handling

### Liquidation System
- Automated liquidation triggers
- Liquidator rewards
- Insurance fund integration
- Position settlement

### Liquidity Provision
- Liquidity pool creation
- LP token management
- Fee accumulation
- Reward distribution

## Monitoring Features
- Protocol statistics
- Position tracking
- Market metrics
- Liquidity pool data

## Administrative Functions
- Market creation
- Price updates
- Fee withdrawal
- Emergency controls

---

Note: This contract implements core perpetual futures trading functionality. Production deployment requires proper price feed integration and thorough testing.