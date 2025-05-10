signature C_STRING = sig
   type t
 
   val isNull: t -> bool
   val size: t -> int
   val sub: t * int -> char
   val toString: t -> string
end

structure CString : C_STRING = struct
   type t = MLton.Pointer.t
	     
   fun isNull s = s =  MLton.Pointer.null

   fun sub (s, i) = Byte.byteToChar ( MLton.Pointer.getWord8 (s, i))
      
   fun size s = let
      fun loop i = if #"\000" = sub (s, i) then i else loop (i + 1)
   in
      loop 0
   end
      
   fun toString s =
      if isNull s then
         raise Fail "CString.toString"
      else
         CharVector.tabulate (size s, fn i => sub (s, i))

end
