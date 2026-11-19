# Objects serialization support
To be done.

## STON
-> [doc](https://github.com/svenvc/ston/blob/master/ston-paper.md)

## Fuel
-> [doc]()

## STON v.s. Fuel

|Property                                            |STON                |Fuel                |
|----------------------------------------------------|--------------------|--------------------|
|Human-readable                                      | :white_check_mark: | :x:                |
|Built-in                                            | :white_check_mark: | :white_check_mark: |
|Works between different major Pharo versions        | :white_check_mark: | :x:*               |
|Does not require configuration on seralized objects | :x:                | :white_check_mark: |

> *It might work but it is not likely. If a fix was done on Fuel during Pharo development (which is likely), it might make your `.fuel` files unreadable by Pharo. STON is more stable from this perspective.
