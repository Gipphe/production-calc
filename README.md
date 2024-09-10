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
ReinforcedIronPlate: 5.0 items/m (1.0 Assembler)
|
+- IronPlate: 30.0 items/m (1.5 Constructor)
|  |
|  `- IronIngot: 45.0 items/m (1.5 Smelter)
|     |
|     `- RawResource IronOre: 45.0 items/m
|
`- Screw: 60.0 items/m (1.5 Constructor)
   |
   `- IronRod: 15.0 items/m (1.0 Constructor)
      |
      `- IronIngot: 15.0 items/m (0.5 Smelter)
         |
         `- RawResource IronOre: 15.0 items/m
```
