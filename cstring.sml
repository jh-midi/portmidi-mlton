signature C_STRING =
sig
    val isNull:  MLton.Pointer.t -> bool
    val strlen:  MLton.Pointer.t -> int
    val sub:  MLton.Pointer.t * int -> char
    val toString:  MLton.Pointer.t -> string
end

structure CString :> C_STRING =
struct

val strlen1 = _import "strlen":  MLton.Pointer.t -> int;

fun isNull s  = s = MLton.Pointer.null

fun sub (s, i) = Byte.byteToChar ( MLton.Pointer.getWord8 (s, i))

fun strlen s = if isNull s then 0 else strlen1 s
					      
fun toString s  =
    if isNull s then
        raise Fail "CString.toString"
    else
        CharVector.tabulate (strlen s, fn i => sub (s, i))
end
