# production-calc

This is a very quirky tool for calculating recipe costs and requirements in
Satisfactory and Factorio. It's best used from the REPL, where you merely
`import` the stuff you need and perform your calculations. It's simplest to
just put all the production lines you want to calculate in either
`Satisfactory.Factories` or `Factorio.Factories` as code, and then do
`ProductionLine.tree Satisfactory.Factories.heatsink`, for example, to get a
nice overview.

Example REPl session:

```text
> import Satisfactory.Factories
> import ProductionLine
> tree reinforcedIronPlate
ReinforcedIronPlate: 5.555555555555555 items/m (1.1111111111111112 Assembler)
|
+- IronPlate: 33.33333333333333 items/m (1.6666666666666665 Constructor)
|  |
|  `- IronIngot: 50.0 items/m (1.6666666666666665 Smelter)
|     |
|     `- RawResource IronOre: 50.0 items/m
|
`- Screw: 66.66666666666666 items/m (1.6666666666666665 Constructor)
   |
   `- IronRod: 16.666666666666664 items/m (1.1111111111111112 Constructor)
      |
      `- IronIngot: 16.666666666666664 items/m (0.5555555555555556 Smelter)
         |
         `- RawResource IronOre: 16.666666666666664 items/m
```
