@@ portable

val make : size:int -> f:(int -> 'a @ portable) -> (int -> 'a @ contended) @ portable
