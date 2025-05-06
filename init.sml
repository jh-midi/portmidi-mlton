open Portmidi;
structure Init =
struct

fun testId (id, name) =
    if id = ~1 andalso name = "diato" then
	raise Fail (name ^ " is missing. Please create the virtual device with port-name " ^ name ^ "\n")
    else if id = ~1  then
	raise Fail (name ^ " is missing. Please connect your Atom SQ \n")
    else ()


	     

val atom_name = if OS.Process.getEnv "OS" = SOME "Windows_NT"
		then "ATM SQ" else  "ATM SQ ATM SQ"

val virtual_name = "diato"

val _ = terminate()
val _ = initialize()
val _ =  if  OS.Process.getEnv "OS" = SOME "Windows_NT" then
	     0 else  createVirtualOutput "diato"
val atom_in_id = getDeviceInputId atom_name
val diato_out_id =  getDeviceOutputId virtual_name
		       
val _ = ( testId (atom_in_id, atom_name);
	  testId (diato_out_id, virtual_name))
		       
val ATOMSQ_out = openOutput (getDeviceOutputId atom_name) 100 0
val ATOMSQ_in =  openInput atom_in_id 100
val DIATO_out = openOutput diato_out_id 100 0
end		       
