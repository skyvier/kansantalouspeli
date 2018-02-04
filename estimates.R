source("functional.R")

# WIP: Effect of Trade on Development

calculateSectorValue <- function (gameKey, stockKey) {
   tradeAmount <- game[[gameKey]]$values
   tradePrice  <- stock[[stockKey]]$values
   return(tradeAmount[-length(tradeAmount)] * tradePrice)
}

calculateTotalValue <- function (quantities, prices) {
   keyPairs   <- zipVectors(quantities, prices)
   totalValue <- rowFoldl(function (acc, x) acc + calculateSectorValue(x[1], x[2]), 0, keyPairs)
   return(totalValue)
}

# Estimates our share of the world market and the effect of trade on development
# based on those shares. The EoToD estimate (calculated_effects) isn't exactly
# correct.
#
# Parameters:
#  game: game data frame from `read.game_table`
#  stock: stock data frame from `read.game_stock`
#  n: amount of countries in the game (19)
estimate_trade <- function (game, stock, n) {
   our_trade <- data.frame(
      farming_export = calculateTotalValue(c("HFE", "LFE", "SE"), c("HFS", "LFS", "SS")),
      farming_import = calculateTotalValue(c("HFI", "LFI", "SI"), c("HFB", "LFB", "SB")),
      industry_export = calculateTotalValue(c("HGE", "LGE"), c("HGS", "LGS")),
      industry_import = calculateTotalValue(c("HGI", "LGI"), c("HGB", "LGB")),
      energy_export = calculateTotalValue(c("FFE", "EE"), c("FFS", "ES")),
      energy_import = calculateTotalValue(c("FFI", "EI"), c("FFB", "EB"))
      )

   total_trade <- data.frame(
      farming_export = stock$LFO$values + stock$HFO$values + stock$SO$values,
      farming_import = stock$LFD$values + stock$HFD$values + stock$SD$values,
      industry_export = stock$LGO$values + stock$HGO$values,
      industry_import = stock$LGD$values + stock$HGD$values,
      energy_export = stock$FFO$values + stock$EO$values,
      energy_import = stock$FFD$values + stock$ED$values
      )

   shares <- our_trade / total_trade
   effects <- game$EoToD$values

   coefs <- 1 / (game$PI$values[-length(game$PI$values)] * (6/n)) 
   calculated_effects <- rowSums(coefs * shares)

   result = list(shares = shares, calculated_effects = calculated_effects, effects = effects) 
   return(result)
}
