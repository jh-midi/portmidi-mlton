open Init;
structure Display =
struct
(* position *)
val TopLeft =  0     (* + 3 for bottom line *)
val TopCenter =  1
val TopRight =  2
val BottomLeft = 8
val BottomCenter =  9
val BottomRight = 10
val TopLeft_ =  3     (* add _ for bottom + 3 for bottom line *)
val TopCenter_ =  4
val TopRight_ =  5
val BottomLeft_ = 11
val BottomCenter_ =  12
val BottomRight_ = 13

val Middle =  6	 (* + 1 = bottom line *)
val Middle_ =  7	
(* alignment *)		  
val Centered = 0
val LeftAligned = 1
val RightAligned = 2

		       
val folder_icon =   0x0A (* "\n" *)
val note_glyph =   0x0B
val power_icon =  0x1B 
val check_mark =  0x1C 
val X_mark =  0x1D 
val thin_circle =  0x1E
val thick_circle =  0x1F
val degree_symbol =  0x7F

type display = {name: string, position:int}
val displays = Array.array(12,{name="", color = [127,127,127], position= ~1})
val actif = ref TopLeft
						       			       (*
sendText "Sol/Do" TopLeft [110,127,0] Centered
sendText "GC" (TopLeft + 3) [110,127,0] 7)
sendText "" (Middle+1) [0,0,0] Centered
RGB is 7 bits     pos color---   e  l  l  o
F0 00 01 06 22 12 05 00 7F 00 00 65 6C 6C 6F F7
									       *)
		
datatype lcd_text = Int of int | String of string
					      
fun sendText (to_display:lcd_text, position :int, rgb:int list,  align:int) =
    let val header =  [0xF0, 0x00, 0x01, 0x06, 0x22, 0x12]
	val position2 = position :: []
	val color = rgb (*  [0x00,0x7F,0x00] *)
	val align2 =  align :: []
	val texte2 =  case to_display of Int symbol => symbol :: []
				  | String texte => map Char.ord (String.explode texte)
	val end_sysex =  [0xF7]
	val final1 = header @ position2 @ color @ align2 @ texte2 @ end_sysex
	val final = Array.fromList (map Word8.fromInt final1)
    in
	writeSysex  ATOMSQ_out (ptTime()) final
    end

end
    
    

