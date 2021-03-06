// バイナリファイルの読み込み
let bin = System.IO.File.ReadAllBytes "../test/kernel"

// バイナリデータの配列インデックス
let mutable i = 0
// 
let show len asm =
    // バイナリデータ配列の位置
    printf "%08X  " i
    // バイナリデータの表示
    for j = 0 to len - 1 do
        printf "%02X" bin.[i + j]
    // バイナリデータとアセンブラの間のインデント
    for j = len to 8 do
        printf "  "
    // アセンブラの表示
    printfn "%s" asm
    // インデックスを1命令分進める
    i <- i + len

// レジスタ
let reg16 = [| "ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di" |]
let reg8  = [| "al"; "cl"; "dl"; "bl"; "ah"; "ch"; "dh"; "bh" |]
let sreg  = [| "es"; "cs"; "ss"; "ds" |]
let regm  = [| "bx+si"; "bx+di"; "bp+si"; "bp+di"
               "si"; "di"; "bp"; "bx" |]

// team3
let team3_accum = [| "al"; "ax" |]
let team3_regw = [| reg8; reg16 |]
let team3_word() =
    int bin.[i] &&& 0b1 
let dispword() =
    let mode = int bin.[i+1] >>> 6
    let w = int bin.[i] &&& 0b1
    if mode = 0b11 then
        ""
    elif w = 0 then
       "byte "
    elif w = 1 then
       "word "
    else
       "??? "
let team3_dispdata d =
    if team3_word() = 0 || int bin.[i+d+1] = 0 then
        sprintf "0x%x" bin.[i+d]
    elif team3_word() = 1 then
        sprintf "0x%x%02x" bin.[i+d+1] bin.[i+d]
    else
        sprintf "???"


// 最下位ビットからbyte or wordを判定
let team3_dispsize d =
    if d &&& 0b00000001 = 0 then
        sprintf "byte"
    else
        sprintf "word"
// 7番目のビットからcountの値を判定
let team3_dispcount d =
    if d &&& 0b00000010 = 0 then
        sprintf "1"
    else
        sprintf "cl"

let getregstr w (bin: byte) = 
    let reg = (int bin >>> 3) &&& 0b111
    if w = 0b0 then reg8.[reg]
    else reg16.[reg]

let dispstr d =
    if d < 0 then
        sprintf "-0x%x" (abs d)
    else
        sprintf "+0x%x" d

let modrm() =
    let mode = int bin.[i+1] >>> 6
    let rm   = int bin.[i+1] &&& 0b111
    let w = int bin.[i] &&& 0b1
    match mode, rm with
    | 0b00, 0b110 ->
        if int bin.[i+3] = 0 then
            2, sprintf "[0x%x]" bin.[i+2]
        else
            2, sprintf "[0x%x%02x]" bin.[i+3] bin.[i+2]
    | 0b00, _ ->
        0, sprintf "[%s]" regm.[rm]
    | 0b01, _ ->
        let d = bin.[i+2] |> sbyte
        1, sprintf "[%s%s]" regm.[rm] (dispstr (int d))
    | 0b10, _ ->
        let d = (int16 bin.[i+2]) ||| ((int16 bin.[i+3]) <<< 8)
        2, sprintf "[%s%s]" regm.[rm] (dispstr (int d))
    | 0b11, _ ->
        match int bin.[i] with
        | 0x8C | 0x8E -> 0, reg16.[rm]
        | _ -> 0, if w = 0 then reg8.[rm] else reg16.[rm]
    | _ ->
        0, "???"

// TODO: disp-low と disp-high がある場合がうまくいかない
let disp len =
    let refindex = i + len
    match len with
    | 2 -> (refindex + (int (sbyte bin.[refindex - 1])))
    | 3 ->
        if int bin.[i] = 0b11101001 then
            (refindex + ((int bin.[refindex - 1] <<< 8) + int bin.[refindex - 2])) % 0x10000
        else
            refindex + ((int bin.[refindex - 1] <<< 8) + int bin.[refindex - 2])
    | _ -> 0

// リトルエンディアンから元の2バイト整数に変換する
let to2byte offset = ((int bin.[i + offset + 2] <<< 8) + int bin.[i + offset + 1])

// Main flow
while i < bin.Length do
    match int bin.[i] with
    // MOV: R/M to/from Register
    | b when b &&& 0b11111100 = 0b10001000 ->
        let dat = bin.[i]
        let d = (int dat >>> 1) &&& 0b1 // to or from
        let w = int dat &&& 0b1 // word or byte

        let reg = (if w > 0 then reg16 else reg8).[((int bin.[i+1] >>> 3) &&& 0b111)]

        let len, opr = modrm()
        show (2 + len) <| if d > 0 then sprintf "mov %s,%s" reg opr else sprintf "mov %s,%s" opr reg
    // MOV: Immediate to R/M (word)
    | 0b11000111 (* C7 *) ->
        let len, opr = modrm()
        let imm = to2byte (len + 1)
        //show (4 + len) <| sprintf "mov word %s,0x%x%x" opr bin.[i+3+len] bin.[i+2+len]
        show (4 + len) <| sprintf "mov word %s,0x%x" opr imm
    // MOV: Immediate to R/M (byte)
    | 0b11000110 ->
        let len, opr = modrm()
        show (3 + len) <| sprintf "mov byte %s,0x%x" opr bin.[i+2+len]
    // POP: R/M
    | 0b10001111 ->
        let len, opr = modrm()
        show (2 + len) <| sprintf "pop %s%s" (if (int bin.[i+1] &&& 0b11000000) = 0b11000000 then "" else "word ") opr
    // POP: Register
    | b when b &&& 0b11111000 = 0b01011000 ->
        show 1 <| sprintf "pop %s" (reg16.[int bin.[i] &&& 0b111])
    // POP: Segment Register
    | b when b &&& 0b11100111 = 0b00000111 ->
        show 1 <| sprintf "pop %s" (sreg.[int bin.[i] >>> 3 &&& 0b11])
    // <<<<<<<<<<<<<<<<<<<<<<
    // IN: Fixed Port w=1
    | 0b11100101 ->
        show 2 <| sprintf "in ax,0x%x" bin.[i+1]
    // IN: Fixed Port w=0
    | 0b11100100 ->
        show 2 <| sprintf "in al,0x%x" bin.[i+1]
    // IN: Variable Port w=1
    | 0b11101101 ->
        show 1 <| sprintf "in ax,dx"
    // IN: Variable Port w=0
    | 0b11101100 ->
        show 1 <| sprintf "in al,dx"
    // XLAT
    | 0b11010111 ->
        show 1 <| sprintf "xlatb"
    // LDS
    | 0b11000101 ->
        let len, opr = modrm()
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        show (2 + len) <| sprintf "lds %s,%s" reg16.[reg] opr
    // LAHF
    | 0b10011111 ->
        show 1 <| "lahf"
    // PUSHF
    | 0b10011100 ->
        show 1 <| "pushfw"
    // <<<<<<<<<<<<<<<<<<<<<<
    // ----------------------
    // MOV Memory to Accumulator
    | b when b &&& 0b11111110 = 0b10100000 ->
        if int bin.[i+2] = 0 then
            show 3 <| sprintf "mov %s,[0x%x]"
                          (if b &&& 0b1 = 0 then "al"; else "ax")
                          bin.[i+1]
        else
            show 3 <| sprintf "mov %s,[0x%x%x]"
                          (if b &&& 0b1 = 0 then "al"; else "ax")
                          bin.[i+2] bin.[i+1]
    // MOV Accumulator to Memory
    | b when b &&& 0b11111110 = 0b10100010 ->
        show 3 <| sprintf "mov [0x%x%x],%s"
                          bin.[i+2] bin.[i+1]
                          (if b &&& 0b1 = 0 then "al"; else "ax") 

    // OUT Fixed value
    | b when b &&& 0b11111110 = 0b11100110 ->
        show 2 <| sprintf "out 0x%x,%s"
                          bin.[i+1]
                          (if b &&& 0b1 = 0 then "al"; else "ax") 
    // OUT Variable port
    | b when b &&& 0b11111110 = 0b11101110 ->
        show 1 <| sprintf "out dx,%s"
                          (if b &&& 0b1 = 0 then "al"; else "ax") 
    // LEA
    | 0b10001101 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show (2 + len) <| sprintf "lea %s,%s"
                                  reg16.[reg] opr
    // LES
    | 0b11000100 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show (2 + len) <| sprintf "les %s,%s"
                                  reg16.[reg] opr
    // PUSH Register/Memory
    | 0b11111111 ->
        let len, opr = modrm()
        let flag = (int bin.[i + 1] >>> 3) &&& 0b111
        let size = dispword()
        let cmd = match flag with
                    | 0b011 -> "call"
                    | 0b110 -> "push"
                    | 0b000 -> "inc"
                    | 0b001 -> "dec"
                    | 0b010 -> "call"
                    | 0b100 -> "jmp"
                    | 0b101 -> "jmp"
                    | _ -> "???"
        show (2 + len) <| sprintf "%s %s%s" cmd size opr

    // PUSH Register
    | b when b &&& 0b11111000 = 0b01010000 ->
        show 1 <| sprintf "push %s"
                          reg16.[b &&& 0b111]
    // PUSH Segment Register
    | b when b &&& 0b11100111 = 0b00000110 ->
        let reg = (int b >>> 3) &&& 0b11
        show 1 <| sprintf "push %s"
                          sreg.[reg]
    // XCHG Register/Memory with Register
    | b when b &&& 0b11111110 = 0b10000110 ->
        let mode = int bin.[i+1] >>> 6
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        let w = if (int b &&& 0b1 = 1) then true else false
        show (2 + len) <| sprintf "xchg %s,%s"
                                  (if w then reg16 else reg8).[reg]
                                  (if (w || (mode <> 0b11)) then
                                    opr
                                   else
                                    reg8.[int bin.[i+1] &&& 0b111])
    | 0b10000111 -> // w = 1
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show (2 + len) <| sprintf "xchg %s,%s"
                                  reg16.[reg] opr
    // NOP
    | 0b10010000 ->
        show (1) <| sprintf "nop"
    // XCHG Register with Accumulator
    | b when b &&& 0b11111000 = 0b10010000 ->
        show 1 <| sprintf "xchg ax, %s"
            reg16.[b &&& 0b111]        
    // SAHF
    | 0b10011110 ->
        show 1 <| sprintf "sahf"
    //POPFW
    | 0b10011101 ->
        show 1 <| sprintf "popfw"
    // ----------------------

    // AND Reg./Memory and Register to Either
    | b when b &&& 0b11111100 = 0b00100000 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        if b &&& 0b10 = 0b00 then
            show (2 + len) <| sprintf "and %s,%s"
                                         opr team3_regw.[team3_word()].[reg]
        if b &&& 0b10 = 0b10 then
            show (2 + len) <| sprintf "and %s,%s"
                                         team3_regw.[team3_word()].[reg] opr
    // OR Reg./Memory and Register to Either
    | b when b &&& 0b11111100 = 0b00001000 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        if b &&& 0b10 = 0b00 then
            show (2 + len) <| sprintf "or %s,%s"
                                         opr team3_regw.[team3_word()].[reg]
        if b &&& 0b10 = 0b10 then
            show (2 + len) <| sprintf "or %s,%s"
                                         team3_regw.[team3_word()].[reg] opr
    // XOR Reg./Memory and Register to Either
    | b when b &&& 0b11111100 = 0b00110000 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        if b &&& 0b10 = 0b00 then
            show (2 + len) <| sprintf "xor %s,%s"
                                         opr team3_regw.[team3_word()].[reg]
        if b &&& 0b10 = 0b10 then
            show (2 + len) <| sprintf "xor %s,%s"
                                         team3_regw.[team3_word()].[reg] opr
    // TEST Register/Memory and Register
    | b when b &&& 0b11111110 = 0b10000100 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show (2 + len) <| sprintf "test %s,%s"
                                            opr team3_regw.[team3_word()].[reg]

    // AND Immediate to Register/Memory
    // OR  Immediate to Register/Memory
    // XOR Immediate to Register/Memory
    | b when b &&& 0b11111110 = 0b10000000 (* 80 / 81 *) ->
        let w = int bin.[i] &&& 0b1
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        let data = team3_dispdata (len + 2)
        let team3_w = dispword()
        if reg = 4 then
            show (3 + len + team3_word()) <| sprintf "and %s%s,%s"
                                                     team3_w opr data
        elif reg = 1 then
            show (3 + len + team3_word()) <| sprintf "or %s%s,%s"
                                                     team3_w opr data
        elif reg = 6 then
            show (3 + len + team3_word()) <| sprintf "xor %s%s,%s"
                                                     team3_w opr data
        elif reg = 0b000 then
            show (3 + len + team3_word()) <| sprintf "add %s%s,%s" team3_w opr data
        elif reg = 0b000 then
            show (3 + len + team3_word()) <| sprintf "adc %s%s,%s" team3_w opr data
        elif reg = 0b000 then
            show (3 + len + team3_word()) <| sprintf "sub %s%s,%s" team3_w opr data
        elif reg = 0b000 then
            show (3 + len + team3_word()) <| sprintf "sbb %s%s,%s" team3_w opr data
        elif reg = 0b111 then
            show (3 + len + team3_word()) <| sprintf "cmp %s%s,%s" team3_w opr data
        else
            show 1 <| sprintf "db 0x%02x" bin.[i]

    // TEST Immediate Data and Register/Memory
    | b when b &&& 0b11111110 = 0b11110110 ->
        let flag = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        let data = team3_dispdata (len + 2)
        let size = dispword()
        //let size = team3_dispsize (int bin.[i])
        let w = int bin.[i] &&& 0b1
        let cmd = match flag with
                    | 0b000 -> "test"
                    | 0b010 -> "not"
                    | 0b011 -> "neg"
                    | 0b100 -> "mul"
                    | 0b101 -> "imul"
                    | 0b110 -> "div"
                    | 0b111 -> "idiv"
                    | _ -> "???"
        match flag with
            | 0b000 -> show (3 + len + w) <| sprintf "%s %s%s,%s" cmd size opr data
            | _     -> show (2 + len) <| sprintf "%s %s%s" cmd size opr

    // AND Immediate to Accumulator
    | b when b &&& 0b11111110 = 0b00100100 ->
        let data = team3_dispdata 1
        show (2 + team3_word()) <| sprintf "and %s,%s"
                                    team3_accum.[team3_word()] data
    // TEST Immediate Data and Accumulator
    | b when b &&& 0b11111110 = 0b10101000 ->
        let data = team3_dispdata 1
        show (2 + team3_word()) <| sprintf "test %s,%s"
                                    team3_accum.[team3_word()] data
    // OR Immediate to Accumulator
    | b when b &&& 0b11111110 = 0b00001100 ->
        let data = team3_dispdata 1
        show (2 + team3_word()) <| sprintf "or %s,%s"
                                    team3_accum.[team3_word()] data
    // XOR Immediate to Accumulator
    | b when b &&& 0b11111110 = 0b00110100 ->
        let data = team3_dispdata 1
        show (2 + team3_word()) <| sprintf "xor %s,%s"
                                    team3_accum.[team3_word()] data

// Shift/Rotate系の分岐
    | b when b &&& 0b11111100 = 0b11010000 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        let count = team3_dispcount (int bin.[i])
        let size = dispword()
        match reg with
        // Shift Logical/Arithmetic Left
        | 0b100 ->
            show (2 + len) <| sprintf "shl %s%s,%s" size opr count
        // Shift Logical Right
        | 0b101 ->
            show (2 + len) <| sprintf "shr %s%s,%s"
                size opr count
        // Shift Arithmetic Rigth
        | 0b111 ->
            show (2 + len) <| sprintf "sar %s%s,%s"
                size opr count
        // Rotate Right
        | 0b000 ->
            show (2 + len) <| sprintf "rol %s%s,%s"
                size opr count
        // Rotate Left
        | 0b001 ->
            show (2 + len) <| sprintf "ror %s%s,%s"
                size opr count
        // Rotate Through Carry Flag Left
        | 0b010 ->
            show (2 + len) <| sprintf "rcl %s%s,%s"
                size opr count
        // Rotate Through Carry Right
        | 0b011 ->
            show (2 + len) <| sprintf "rcr %s%s,%s"
                size opr count
        | _ ->
            show 1 <| sprintf "db 0x%02x" bin.[i]
// REP系の分岐
    | b when b &&& 0b11111110 = 0b11110010 ->
        let opr = if b &&& 0b1 = 0 then "repne" else "rep"
        if i = bin.Length - 1 then
            show 1 <| sprintf "%s" opr
        elif bin.[i] = bin.[i+1] then
            i <- i + 1
        else
            match (int bin.[i+1]) &&& 0b11111110 with
            // REP MOVS
            | 0b10100100 ->
                show 2 <| sprintf "%s movs%c" opr (team3_dispsize(int bin.[i+1])).[0]
            // REP CMPS
            | 0b10100110 ->
                if opr = "rep" then
                    show 2 <| sprintf "%se cmps%c" opr (team3_dispsize(int bin.[i+1])).[0]
                else
                    show 2 <| sprintf "%s cmps%c" opr (team3_dispsize(int bin.[i+1])).[0]
            // REP SCAS
            | 0b10101110 ->
                if opr = "rep" then
                    show 2 <| sprintf "%se scas%c" opr (team3_dispsize(int bin.[i+1])).[0]
                else
                    show 2 <| sprintf "%s scas%c" opr (team3_dispsize(int bin.[i+1])).[0]
            // REP LODS
            | 0b10101100 ->
                show 2 <| sprintf "%s lods%c" opr (team3_dispsize(int bin.[i+1])).[0]
            // REP STOS
            | 0b10101010 ->
                show 2 <| sprintf "%s stos%c" opr (team3_dispsize(int bin.[i+1])).[0]
            | _ ->
                show 1 <| sprintf "%s" opr
// MOVSの分岐
    | b when b &&& 0b11111110 = 0b10100100 ->
        show 1 <| sprintf "%s%c" "movs" (team3_dispsize b).[0]
// CMPSの分岐
    | b when b &&& 0b11111110 = 0b10100110 ->
        show 1 <| sprintf "%s%c" "cmps" (team3_dispsize b).[0]
// SCASの分岐
    | b when b &&& 0b11111110 = 0b10101110 ->
        show 1 <| sprintf "%s%c" "scas" (team3_dispsize b).[0]
// LODSの分岐
    | b when b &&& 0b11111110 = 0b10101100 ->
        show 1 <| sprintf "%s%c" "lods" (team3_dispsize b).[0]
// STOSの分岐
    | b when b &&& 0b11111110 = 0b10101010 ->
        show 1 <| sprintf "%s%c" "stos" (team3_dispsize b).[0]
// CALLの分岐その1(Direct within Segment)
    | 0b11101000 ->
        let d = (int16 bin.[i+1]) ||| ((int16 bin.[i+2]) <<< 8)
        show 3 <| sprintf "call word 0x%x" (d + int16 (i + 3))
// CALLの分岐その2(Direct Intersegment)
    | 0b10011010 ->
        let off = (int16 bin.[i+1]) ||| ((int16 bin.[i+2]) <<< 8)
        let seg = (int16 bin.[i+3]) ||| ((int16 bin.[i+4]) <<< 8)
        show 5 <| sprintf "call word 0x%x:0x%x" (int16 seg) (int16 off)
// CALLの分岐その3(Indirect within Segment or Indirect Intersegment)
    | 0b11111111 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        match reg with
        // Indirect within Segment
        | 0b010 ->
            show (2 + len) <| sprintf "call word %s" opr
        // Indirect Intersegment
        | 0b011 ->
            show (2 + len) <| sprintf "call word far %s" opr
        | _ ->
            show 1 <| sprintf "db 0x%02x" bin.[i]
        
    | 0b10001110 ->
        let seg = (int bin.[i+1] >>> 3) &&& 0b11
        let len, opr = modrm()
        show (2 + len) <| sprintf "mov %s,%s" sreg.[seg] opr
    | 0b10001100 ->
        let seg = (int bin.[i+1] >>> 3) &&& 0b11
        let len, opr = modrm()
        show (2 + len) <| sprintf "mov %s,%s" opr sreg.[seg]
    | b when b &&& 0b11111000 = 0b10110000 ->
        show 2 <| sprintf "mov %s,0x%x" reg8.[b &&& 0b111] bin.[i+1]

    // MOV Immediate to Register
    | b when b &&& 0b11111000 = 0b10111000 (* B8 *) ->
        let value = (int bin.[i+2] <<< 8) + int bin.[i+1]
        show 3 <| sprintf "mov %s,0x%x" reg16.[b &&& 0b111] value


    // Add: R/M with R to Either
    | b when (b >>> 2) = 0b000000 ->
        let d = (int bin.[i] >>> 1) &&& 0b1
        let w = (int bin.[i]) &&& 0b1
        let len, op = modrm()
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let fromreg = if d = 0 then (getregstr w bin.[i+1]) else op
        let toreg   = if d = 1 then (getregstr w bin.[i+1]) else op
        show (2 + len) <| sprintf "add %s,%s" toreg fromreg

    // Add with Carry: R/M with R to Either
    | b when (b >>> 2) = 0b000100 ->
        let d = (int bin.[i] >>> 1) &&& 0b1
        let w = (int bin.[i]) &&& 0b1
        let len, op = modrm()
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let fromreg = if d = 0 then (getregstr w bin.[i+1]) else op
        let toreg   = if d = 1 then (getregstr w bin.[i+1]) else op
        show (2 + len) <| sprintf "adc %s,%s" toreg fromreg

    // Subtract: R/M with R to Either dw=00
    | b when (b >>> 2) = 0b001010 ->
        let d = (int bin.[i] >>> 1) &&& 0b1
        let w = (int bin.[i]) &&& 0b1
        let len, op = modrm()
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let fromreg = if d = 0 then (getregstr w bin.[i+1]) else op
        let toreg   = if d = 1 then (getregstr w bin.[i+1]) else op
        show (2 + len) <| sprintf "sub %s,%s" toreg fromreg

    // Add: Immediate to R/M sw=11
    // Add with Carry: Immediate to R/M sw=11
    // Subtract: Immediate to R/M sw=11
    | 0b10000011 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, op = modrm()
        let size = dispword()
        let signed_disp = dispstr (int (sbyte bin.[i+len+2]))
        let cmd = 
            match reg with
                // Add
                | 0b000 -> "add"
                // Add with Carry
                | 0b010 -> "adc"
                // Subtract
                | 0b101 -> "sub"
                | 0b011 -> "sbb"
                | 0b111 -> "cmp"
                | _ -> "???"
        show (3 + len) <| sprintf "%s %s%s,byte %s" cmd size op signed_disp

    // Add: Immediate to Accumulator w=0
    | 0b00000100 ->
        show 2 <| sprintf "add al,0x%x" bin.[i+1]
    // Add: Immediate to Accumulator w=1
    | 0b00000101 ->
        if int bin.[i+2] = 0 then
            show 3 <| sprintf "add ax,0x%x" bin.[i+1]
        else
            show 3 <| sprintf "add ax,0x%x%x" bin.[i+2] bin.[i+1]
    // Add with Carry: Immediate to Accumulator w=0
    | 0b00010100 ->
        show 2 <| sprintf "adc al,0x%x" bin.[i+1]
    // Add with Carry: Immediate to Accumulator w=1
    | 0b00010101 ->
        show 3 <| sprintf "adc ax,0x%x%x" bin.[i+2] bin.[i+1]    
    // Increment: Register
    | b when (b >>> 3) = 0b01000 ->
        let reg = (int b) &&& 0b111
        show 1 <| sprintf "inc %s" reg16.[reg]
    // Increment: R/M w=0
    | 0b11111110 ->
        let reg = (int bin.[i + 1] >>> 3) &&& 0b111
        let len, op = modrm()
        if reg = 0b000 then
            show (2 + len) <| sprintf "inc byte %s" op
        elif reg = 0b001 then
            show (2 + len) <| sprintf "dec byte %s" op
        else show 1 <| sprintf "db 0x%02x" bin.[i]
    // Increment: R/M w=1
    | 0b11111111 ->
        let len, op = modrm()
        let reg = (int bin.[i + 1] >>> 3) &&& 0b111
        if reg = 0b000 then
            show (2 + len) <| sprintf "inc word %s" op
        elif reg = 0b001 then
            show (2 + len) <| sprintf "dec word %s" op
        else show 1 <| sprintf "db 0x%02x" bin.[i]
    // AAA: ASCII Adjust for Add
    | 0b00110111 -> show 1 <| sprintf "aaa"
    // DAA: Decimal Adjust for Add
    | 0b00100111 -> show 1 <| sprintf "daa"
    // Subtract: Immediate to Accumulator w=0
    | 0b00101100 ->
        show 2 <| sprintf "sub al, 0x%x" bin.[i+1]
    // Subtract: Immediate to Accumulator w=1
    | 0b00101101 ->
        if int bin.[i+2] = 0 then
            show 3 <| sprintf "sub ax,0x%x" bin.[i+1]
        else
            show 3 <| sprintf "sub ax,0x%x%x" bin.[i+2] bin.[i+1]
    // sbb
    | 0x1B ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show (2 + len) <| sprintf "sbb %s,%s" opr reg8.[reg]
    | 0b00011001 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show 2 <| sprintf "sbb %s,%s" opr reg16.[reg]
    | 0b00011010 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show 2 <| sprintf "sbb %s,%s" reg8.[reg] opr
    | 0b00011011 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show 2 <| sprintf "sbb %s,%s" reg16.[reg] opr
    | 0b00011100 ->
        let len, opr = modrm()
        show (2 + len) <| sprintf "sbb al,0x%x" bin.[i+1] 
    | 0b00011101 ->
        let len, opr = modrm()
        show (3 + len) <| sprintf "sbb ax,0x%x%x" bin.[i+2] bin.[i+1] 
    // dec
    | b when (b >>> 3) = 0b01001 ->
       let reg = b &&& 0b111
       show 1 <| sprintf "dec %s" reg16.[reg]
    // cmp, sbb
    | 0b00111000 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show 2 <| sprintf "cmp %s,%s"
                                    opr reg8.[reg]
    
    // CMP 
    | 0x39 ->
        let reg = (int bin.[i+1] >>> 3) &&& 0b111
        let len, opr = modrm()
        show (2 + len) <| sprintf "cmp %s,%s" opr reg16.[reg]
    | 0b00111100 ->
        show 2 <| sprintf "cmp al,0x%x" bin.[i+1]
    | 0b00111101 ->
        if int bin.[i+2] = 0 then
            show 3 <| sprintf "cmp ax,0x%x" bin.[i+1]
        else
            show 3 <| sprintf "cmp ax,0x%x%x" bin.[i+2] bin.[i+1]
    | 0b00111010 -> 
        let len, opr = modrm()
        show 2 <| sprintf "cmp al,%s"
                                   opr
    | 0b00111011 -> 
        let len, opr = modrm()
        show 2 <| sprintf "cmp ax,%s"
                                   opr
    // aas
    | 0b00111111 ->
        show 1 <| sprintf "aas"
    // das
    | 0b00101111 ->
        show 1 <| sprintf "das"
    // aam
    | 0b11010100 ->
        show 2 <| sprintf "aam"

    // aad
    | 0b11010101 ->
        show 2 <| sprintf "aad 0xb"
    // cbw
    | 0b10011000 ->
        show 1 <| sprintf "cbw"
    // cwd
    | 0b10011001 ->
        show 1 <| sprintf "cwd"

    // RET Return from CALL
    | 0xC3 ->
        let len = 1
        show len <| sprintf "ret"
    
    // RET Return from CALL
    | 0xC2 ->
        let len = 3
        show len <| sprintf "ret 0x%x" (to2byte 0)
    
    // JMP Direct within Segment
    | 0b11101001 (* E9 *) ->
        let len = 3
        show len <| sprintf "jmp word 0x%x" (disp len)

    // JMP Direct within Segment-Short
    // TODO: EB5F jmp short 0x106 が 0x106 と2バイト分あるのはおかしい
    | 0b11101011 ->
        let len = 2
        show 2 <| sprintf "jmp short 0x%x" (disp len)
    
    // JZ
    | 0b01110100 ->
        let len = 2
        show 2 <| sprintf "jz 0x%x" (disp len)

    // JBE / JNA
    | 0x76 ->
        let len = 2
        show len <| sprintf "jna 0x%x" (disp len)

    // JL / JNGE
    | 0x7C ->
        let len = 2
        show len <| sprintf "jl 0x%x" (disp len)

    // JO
    | 0x70 ->
        let len = 2
        show len <| sprintf "jo 0x%x" (disp len)

    // JNL / JGE
    | 0x7D ->
        let len = 2
        show len <| sprintf "jnl 0x%x" (disp len)

    // JC / JB / JNAE
    | 0x72 ->
        let len = 2
        show len <| sprintf "jc 0x%x" (disp len)

    // JNC
    | 0b01110011 ->
        let len = 2
        show 2 <| sprintf "jnc 0x%x" (disp len)
    
    // JNBE / JA
    | 0x77 ->
        let len = 2
        show len <| sprintf "ja 0x%x" (disp len)

    // JNZ
    | 0b01110101 ->
        let len = 2
        show 2 <| sprintf "jnz 0x%x" (disp len)
    
    // JNLE/JG
    | 0b01111111 (* 7F *) ->
        let len = 2
        show len <| sprintf "jg 0x%x" (disp len)
    
    // JNG
    | 0b01111110 ->
        let len = 2
        show 2 <| sprintf "jng 0x%x" (disp len)

    // HLT: Halt
    | 0b11110100 ->
        show 1 <| sprintf "hlt"

    // LOOP
    | 0xE2 ->
        let len = 2
        show len <| sprintf "loop 0x%x" (disp len)

    // JCXZ
    | 0xE3 ->
        let len = 2
        show len <| sprintf "jcxz 0x%x" (disp len)

    // INT Interrupt Type Specified
    | 0xCD ->
        let len = 2
        show len <| sprintf "int 0x%x" bin.[i + 1]

    // INT Interrupt Type 3
    | 0xCC ->
        let len = 1
        show len <| sprintf "int"

    // CLD Clear Direction
    | 0xFC ->
        let len = 1
        show len <| sprintf "cld"

    // STD Set Direction
    | 0xFD ->
        let len = 1
        show len <| sprintf "std"

    | 0xFA ->
        let len = 1
        show len <| sprintf "cli"

    | 0xFB ->
        let len = 1
        show len <| sprintf "sti"

    | 0x9D ->
        let len = 1
        show len <| sprintf "popfw"

    | 0xCF ->
        let len = 1
        show len <| sprintf "iretw"

    // Segment Override Prefix
    | b when b &&& 0b11100111 = 0b00100110 ->
        let reg = (int bin.[i] >>> 3) &&& 0b11
        show 1 <| sprintf "%s:" sreg.[reg]

    // push byte
    | 0x6A ->
        let len = 2
        show len <| sprintf "push byte %s" (dispstr (int (sbyte bin.[i+1])))

    | 0x68 ->
        let len = 3
        show len <| sprintf "push word 0x%x" (to2byte 0)

    // pushaw
    | 0x60 ->
        let len = 1
        show len <| sprintf "pushaw"

    // popaw
    | 0x61 ->
        let len = 1
        show len <| sprintf "popaw"

    // outfw
    | 0x6F ->
        let len = 1
        show len <| sprintf "outsw"

    // salc
    | 0xD6 ->
        let len = 1
        show len <| sprintf "salc"

    | _ ->
        show 1 <| sprintf "db 0x%02x" bin.[i]
    