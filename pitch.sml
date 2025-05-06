structure Pitch =
struct



datatype pitch_class =   C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb
			 | G | Gs | Ab | A | As | Bb | B    (* Cs = C sharp => C#*)

type velocity = int							   
type octave = int
		  
type pitch = pitch_class * octave


fun fromString note =
  ( case note of
     "C" => SOME C 
   | "C#" => SOME Cs 
   | "Db" => SOME Db 
   | "D" => SOME D 
   | "D#" => SOME Ds 
   | "Eb" => SOME Eb      
   | "E" => SOME E 
   | "F" => SOME F 
   | "F#" => SOME Fs 
   | "Gb" => SOME Gb 
   | "G" => SOME G 
   | "G#" => SOME Gs 
   | "Ab" => SOME Ab 
   | "A" => SOME  A 
   | "A#" => SOME As 
   | "Bb" => SOME Bb 
   | "B" => SOME B 
   | _ =>  NONE
  )

(*
toString (Cs,4);
=> "C4" 
*)
fun toString ((note: pitch_class), (oct: octave)) :string =
    let val noteStr = case note of
			  C => "C"
			| Cs => "C#"
			| Db => "Db"
			| D => "D"
			| Ds => "D#"
			| Eb => "Eb"      
			| E => "E"
			| F => "F"
			| Fs => "F#"
			| Gb => "Gb"
			| G => "G"
			| Gs => "G#"
			| Ab => "Ab"
			| A => "A"
			| As => "A#"
			| Bb => "Bb"
			| B => "B"
    in
	noteStr ^ Int.toString oct
    end
    
fun note2midi0 (note: pitch_class) :int =
 ( case note of
      C => 0
   |  Cs => 1
   | Db => 1
   | D => 2
   | Ds => 3
   | Eb => 3      
   | E => 4
   | F => 5
   | Fs => 6
   | Gb => 6
   | G => 7
   | Gs => 8
   | Ab => 8
   | A => 9
   | As => 10
   | Bb => 10
   | B => 11)



     
fun pitch2midi (n: pitch_class, oct: octave) : int =
    note2midi0(n) + oct * 12
	
(* note2midi "C2" *)
fun note2midi (note_in :string) :int =
      let val taille = size note_in
	  val name = substring (note_in, 0 , taille - 1)
	  val octave = getOpt(Int.fromString(substring (note_in, 1 , taille - 1)),
			      20) (* no note exist at 20° octave *)
          in
	  case fromString name of
	      SOME(note) => pitch2midi (note,octave)
	   |  NONE => 1000
      end

end
