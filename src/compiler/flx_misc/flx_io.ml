open Batteries

let get_lines input l1' c1 l2' c2 =
  let l1 = max 1 l1' in
  let l2 = l2' + 1 in
  let n = String.length (string_of_int l2) in
  let fmt i =
    let s = "    " ^ string_of_int i in
    let m = String.length s in
    String.sub s (m-n) n
  in

  let buf = Buffer.create ((l2-l1+4) * 80) in
  let spc () = Buffer.add_char buf ' ' in
  let star () = Buffer.add_char buf '*' in
  let nl () = Buffer.add_char buf '\n' in

  for i = 1 to l1-1 do ignore (IO.read_line input) done;
  let too_long = l2'-l1' > 20 in

  begin try
    for i = l1 to l2 do
      let s = IO.read_line input in
      if too_long && i = l1'+3 then
        Buffer.add_string buf ("...\n")
      else if too_long && i > l1'+3 && i< l2'-3 then () else
      begin
        Buffer.add_string buf (fmt i ^": ");
        Buffer.add_string buf s;
        nl();
        if i = l1' && l1' = l2' then
        begin
          for i = 1 to n + 2 do spc () done;
          for i = 1 to c1 - 1 do spc () done;
          for i = c1 to c2 do star () done;
          nl()
        end
      end
    done
  with IO.No_more_input -> ()
  end;

  Buffer.contents buf
