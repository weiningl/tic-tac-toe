Tic-Tac-Toe

- Goal: A project to explore generic programming in Haskell.
- The design starts by thinking about the definition of turn-based games in general. 
  - It should have a state representation (GameState) that can tell end-of-game condition is met, and advance to the next state base on input. Input is abstracted as a computation (monad).
  - Executing a turn is conditionally advancing the state.
- A two-player game play should have one-to-one mapping to True/False.
- A board is a two-dimensional game state.
- Tic-tac-toe is a play combining the concepts above, namely a turn-based, two-player game on a two-dimensional board.
- Add the feature so that computer can decide the next move. Two strategies are implemented:
  - Random move: Choose randomly from all legal moves.
  - Min-max move: Expand the game tree on a zero-sum game to determine the best next move. The simple implementation grows exponentially with the number of states and is not suitable for large boards. Techniques like alpha-beta pruning or limited-depth+heuristics need to be applied for larger game play.