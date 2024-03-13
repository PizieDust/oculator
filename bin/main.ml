open Bogue
module W = Widget
module L = Layout
module T = Trigger
module Math = Oculator.Math

let num_button_style = Style.color_bg (36,41,51,255)
let op_button_style = Style.color_bg (0, 96, 229, 255)
let control_button_style = Style.color_bg (54, 62, 76, 255)

let screen_bg = Style.color_bg (22, 26, 32, 255)
let screen_style = Style.create ~background:screen_bg ()

let board_bg = L.color_bg (22, 26, 32, 255)

let screen = W.label ~size:30 ~fg:(255,255,255,255) "" ~align:Draw.Max
let screen_ans = W.label ~size:50 ~fg:(255,255,255,255) "" ~align:Draw.Max

let setLabel key =
  Label.create ~size:35 key ~fg:(255,255,255,255)

let setIcon icon = 
  Label.icon ~size:35 ~fg:(255,255,255,255) icon

let setButton key style = 
  W.button ~border_radius:5 ~label:(setLabel key) ~bg_on:style ~bg_off:style ""

let setIconButton icon style = 
  W.button ~border_radius:5 ~label:(setIcon icon) ~bg_on:style ~bg_off:style ""

let handle_calc key_equal screen _ =
  let _button_text = W.get_text key_equal in
  let screen_ans_text = W.get_text screen_ans in 
  match screen_ans_text with
  | "" -> 
    let computation = W.get_text screen in
    let result = Math.interp computation in
    W.set_text screen_ans result;
  | text -> 
    W.set_text screen text;
    W.set_text screen_ans ""

let handle_click button screen _ = 
  let button_text = Widget.get_text button in 
  let screen_current_text = W.get_text screen in 
  W.set_text screen (screen_current_text ^ button_text)

let reset_screen button screen _ = 
  let _button = button in 
  W.set_text screen "";
  W.set_text screen_ans ""

let clear_op button screen _ = 
  let _button = button in 
  W.set_text screen_ans "";
  let screen_text = W.get_text screen in 
  W.set_text screen (String.sub screen_text 0 ((String.length screen_text) - 1))

let button_click button = 
  W.connect button screen handle_click ~priority:Join ~update_target:true Trigger.buttons_down

let square_val _key_squared screen _ = 
  let value = W.get_text screen in 
  let result = Math.square (int_of_string value) in 
  W.set_text screen_ans (string_of_int result)

let key_0 = setButton "0" num_button_style
let key_1 = setButton "1" num_button_style
let key_2 = setButton "2" num_button_style
let key_3 = setButton "3" num_button_style
let key_4 = setButton "4" num_button_style
let key_5 = setButton "5" num_button_style
let key_6 = setButton "6" num_button_style
let key_7 = setButton "7" num_button_style
let key_8 = setButton "8" num_button_style
let key_9 = setButton "9" num_button_style
let key_plus = setButton "+" op_button_style
let key_minus = setButton "-" op_button_style
let key_div = setButton "/" op_button_style
let key_mul = setButton "x" op_button_style
let key_equal = setButton "=" control_button_style
let key_percent = setButton "%" control_button_style
let key_fact = setButton "!" control_button_style
let key_dot = setButton "." control_button_style
let key_reset = setButton "AC" control_button_style
let key_back = setIconButton "long-arrow-left" control_button_style
let key_squared = setIconButton "superscript" control_button_style

let ac_click = 
  W.connect key_reset screen reset_screen ~priority:Join ~update_target:true Trigger.buttons_down

let back_click = 
  W.connect key_back screen clear_op ~priority:Join ~update_target:true Trigger.buttons_down

let equal_click =
  W.connect key_equal screen handle_calc ~priority:Join T.buttons_down

let square_click = 
  W.connect key_squared screen square_val ~priority:Join T.buttons_down 

let layer_0 = L.flat (List.map (fun key -> L.resident ~w:185 ~h:70 key) [key_reset; key_back])
let layer_1 = L.flat (List.map (fun key -> L.resident ~w:90 ~h:90 key) [key_9; key_8; key_7; key_plus])
let layer_2 = L.flat (List.map (fun key -> L.resident ~w:90 ~h:90 key) [key_6; key_5; key_4; key_minus])
let layer_3 = L.flat (List.map (fun key -> L.resident ~w:90 ~h:90 key) [key_3; key_2; key_1; key_mul])
let layer_4 = L.flat (List.map (fun key -> L.resident ~w:90 ~h:90 key) [key_dot; key_0; key_percent; key_div])
let layer_5 = L.flat (List.map (fun key -> L.resident ~w:90 ~h:90 key) [key_fact; key_squared] @ [(L.resident ~w:185 ~h:90 key_equal)])
let button_layers = L.tower ~margins:(-15) [layer_0; layer_1; layer_2; layer_3; layer_4; layer_5]
let screen_layer = L.tower ~background:(L.style_bg screen_style) (List.map (fun scr -> L.flat [L.resident ~w:320 ~h:50 scr]) [screen; screen_ans])

let key_clicks = 
  List.map (fun key -> button_click key) [key_0; key_1; key_2; key_3; key_4; key_5; key_6; key_7; key_8; key_9; key_plus; key_minus; key_div; key_mul; key_fact; key_percent; key_dot]

let board_layout = L.tower ~name:"OCulator" ~background:board_bg ~margins:20 [screen_layer; button_layers]

let main () =
  let board = Bogue.of_layout ~connections:(square_click::equal_click::back_click::ac_click::key_clicks) board_layout in 
  Bogue.run board

let _ = main ();
Bogue.quit