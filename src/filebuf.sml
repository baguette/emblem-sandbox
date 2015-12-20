
structure FileBuf = struct
  datatype filebuf = Buf of {
    line_num: int ref,
    col_num: int ref,
    stream: TextIO.instream,
    filename: string
  }
  
  fun new filename =
    let val stream = TextIO.openIn filename
    in
      Buf {
        line_num = ref 1,
        col_num = ref 1,
        stream = stream,
        filename = filename
      }
    end

  fun bump fb =
    let val Buf {line_num, col_num, stream, ...} = fb
        val c = TextIO.input1 stream
    in
      case c of
        NONE     => ()
      | SOME(ch) =>
        if ch = #"\n" then
          (line_num := !line_num + 1;
           col_num := 1)
        else
          col_num := !col_num + 1
    end

  fun getch fb =
    let val Buf {stream, ...} = fb
        val c = TextIO.lookahead stream
    in
      c
    end
end

