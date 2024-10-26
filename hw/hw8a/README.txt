What design choices we’re keeping or changing
* Kept generic List-of X for reusability in many situations (rows, board)
   * Continue using List-of X for all list definitions like lists of blocks
* We both have a position/coordinate data definition with the same fields
   * Matthew’s system started at 1, Raymond’s started at 0
   * We agree that we should start at 0 because it avoids easy off by one errors
* Create data definition for block for future extensibility
* We both have similar data definitions for a row of blocks and a QBert Board
   * Matthew’s QBert board definition went from the top row to the bottom row, Raymond’s was the opposite
   * We agree that going from the top row to the bottom row makes more sense
* Keep the data definition for diagonal directions 
* Raymond’s functions used more list abstractions, Matthew’s used more recursion
   * When possible, list abstractions make more sense
* Raymond’s Draw Board function takes in a count and uses the same color for every block
   * We should avoid this and use the count of each individual block for the color


Comparing our data definitions and the examples
* We both specified a coordinate system with rows and columns
* We both defined a board to be a nested list of list of blocks
   * Matthew made a row a separate data definition so that individual functions could run on rows
* We both tracked the player’s position in a QBertLevel
   * Raymond’s QBertLevel contained the level number and Matthew’s didn't (keeping track of that wouldn’t hurt)
* Matthew’s did not define what a block is, and Raymond’s defined a block as a Natural instead of a struct
   * Why: we both made the mistake of not thinking about extensibility when designing the board definition
   * We are changing our data definitions and functions to include a block as a struct with like the provided countdown


Plan for updating the code design before adding features
* We start with Matthew’s code and make additional changes to improve the design
* Changes based on Raymond’s design
   * Make positions start at 0
   * Reverse the order of QBertBoard to go top row to bottom row
   * Create a struct data definition for the block
   * Modify functions to use list abstractions when possible
   * Add a level number to QBertLevel
   * Change PositiveInteger to Natural
   * Replace quasiquotes with lists
* Taking into grader feedback on homework 5b
   * Change List-of-ITEM to {X} List-of-X
   * Create a struct data definition for the block with examples and template
   * Explain how coordinate system works
   * Explain what a QBL represents
   * Update qbb-temp to call qbr-temp, qbl-temp to call qbb-temp
   * Split move-qbert up into helper functions because it does too much
   * Reduce the number of local definitions in move-qbert
   * Explain what the win condition is
   * In general, move some local definitions out into helper functions


Necessary data definitions while adding features
* Decrementing the block stomps did not require changing any data definitions
* Making QBert face in the direction he most recently moved required adding the last-moved direction to the level data definition
   * This was needed so that the last-moved direction could be accessed in draw-level
* Adding a cycling block required turning the block data definition into an enumerated type with multiple potential structure types, representing the different block types
   * The countdown block remained the same
   * The cycling block requires two naturals: one to represent the current count and one to represent the initial count so it can reset to the correct count
* Adding enemies required adding a list of enemies to the level data definition
   * This required creating a new data definition for an enemy, which could be either a wanderer or a fixer
   * Both types of enemies only require a current position
   * Spawning enemies also required adding the tick count and number of ticks per enemy spawn to the level data definition