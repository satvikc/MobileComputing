* Building project (required ghc > 7.6 and cabal installed)

> cabal configure

> cabal build

* Running MSC

> cd dist/build/MSC

> ./MSC 4001

here 4001 is the port on which to run. You can run another msc on 4002.

> ./MSC 4002

* Running Mobile

> cd dist/build/Mobile

> ./Mobile

* Interacting

> addtohlr 4001 

this will add the mobile to the hlr record of MSC 4001

> changecell 5

this will change the cell to 5

> changemsc 4002

this will change the msc to 4002

