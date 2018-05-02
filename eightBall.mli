open Data

(* [next_state initial_state events] returns the logic state that results
from the advancing the state through the list of events in order*)
val next_state: logic_state -> event list -> logic_state
