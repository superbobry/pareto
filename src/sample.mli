val min    : float array -> float
val max    : float array -> float
val minmax : float array -> (float * float)

val mean     : float array -> float
val variance : ?mean:float -> float array -> float

val histogram
  :  ?bins:int
  -> ?range:(float * float)
  -> ?weights:float array
  -> ?density:bool
  -> float array
  -> float array
