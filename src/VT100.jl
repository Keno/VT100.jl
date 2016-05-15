VERSION >= v"0.4.0-dev+6641" && __precompile__()

module VT100

using Colors
using FixedPointNumbers

typealias RGB8 RGB{UFixed8}

export ScreenEmulator, LineEmulator, Emulator, parse!, parse_cell!, Cell,
    parseall!
import Base: convert, write
import Base.Terminals: cmove_right
import Base: start, next, done, setindex!, getindex, endof

module Attributes
    export Bright, Dim, Underscore, Blink,
        Reverse, Hidden, Italics, IsACS
    const Bright        = 0x1
    const Dim           = 0x2
    const Underscore    = 0x4
    const Blink         = 0x8
    const Reverse       = 0x10
    const Hidden        = 0x20
    const Italics       = 0x40
    const IsACS         = 0x80
end
using .Attributes

module Flags
    export FG_IS_256, BG_IS_256,
           FG_IS_RGB, BG_IS_RGB,
           CELL_IS_IMG
    const FG_IS_256     = 0x1
    const BG_IS_256     = 0x2
    const FG_IS_RGB     = 0x4
    const BG_IS_RGB     = 0x8
    const CELL_IS_IMG   = 0x10
end

# One terminal cell on the screen. Needs to take care of
# the contents of the screen as well as the coloring.
immutable Cell
    # The content to be drawn in this Cell. Unfortunately a terminal emulator
    # also needs to take care of unicode combining characters, which do not fit
    # in a Char. We assign those characters to one of the private use planes
    # (PUP15) and maintain a separate Array of sequences to be filled in when
    # encountering one of these.
    content::Char
    flags::UInt8
    fg::UInt8
    bg::UInt8
    attrs::UInt8
    fg_rgb::RGB8
    bg_rgb::RGB8
end
Cell(c::Cell;
        content = c.content, flags = c.flags, fg = c.fg, bg = c.bg,
        attrs = c.attrs, fg_rgb = c.fg_rgb, bg_rgb = c.bg_rgb) =
    Cell(content,flags,fg,bg,attrs,fg_rgb,bg_rgb)
Cell(c::Char) = Cell(c,0,0,0,0,RGB{UFixed8}(0,0,0),RGB{UFixed8}(0,0,0))

# Encode x information if foreground color, y information in background color
# r encodes the lowest 8 bits, g the next, b the hight bits
function color_for_int(x)
    @assert (x & ~0xFFFFFF) == 0
    RGB8(
        UFixed8(UInt8( x      & 0xFF),nothing),
        UFixed8(UInt8((x>> 8) & 0xFF),nothing),
        UFixed8(UInt8((x>>16) & 0xFF),nothing)
    )
end

function decode_color(c::RGB8)
    UInt8(c.r.i) | (UInt32(UInt8(c.g.i)) << 8) | (UInt32(UInt8(c.b.i)) << 16)
end

function get_image_cell(c::Cell, x, y)
    Cell(c, fg_rgb = color_for_int(x),
        bg_rgb = color_for_int(y), flags = Flags.CELL_IS_IMG)
end

function pos_for_image_cell(c::Cell)
    (decode_color(c.fg_rgb),decode_color(c.bg_rgb))
end

type Line
    data::Vector{Cell}
    wrapped::Bool
    Line() = new(Vector{Cell}(0),false)
    Line(data::Vector{Cell}) = new(data,false)
end
start(l::Line) = start(l.data)
next(l::Line,i) = next(l.data,i)
done(l::Line,i) = done(l.data,i)
setindex!(l::Line, c, i) = setindex!(l.data, c, i)
getindex(l::Line,i) = getindex(l.data,i)
endof(l::Line) = endof(l.data)

convert(::Type{Line}, data::Vector{Cell}) = Line(data)

immutable Cursor
    line::Int
    column::Int
end

immutable Size
    width::Int
    height::Int
end

abstract Emulator

type ScreenEmulator <: Emulator
    ViewPortSize::Size
    firstline::Int
    ExtendedContents::Vector{UTF8String}
    lines::Vector{Line}
    cursor::Cursor
    cur_cell::Cell
    debug::Bool
    linedrawing::Bool
    function ScreenEmulator(width = 80, height = 24)
        this = new(Size(width, height),1,
            Vector{UTF8String}(0),Vector{Line}(0),Cursor(1,1),Cell('\0'),
            false, false)
        add_line!(this)
        this
    end
end
create_cell(em::ScreenEmulator,c::Char) = Cell(em.cur_cell, content = c)
cur_cell(em::ScreenEmulator) = em.cur_cell
set_cur_cell(em::ScreenEmulator,c::Cell) = em.cur_cell = c
function cmove(em::ScreenEmulator, line, col)
    em.debug && println("Moving to $line:$col")
    targetline = em.firstline-1+line
    for _ in targetline:-1:(length(em.lines)+1)
        add_line!(em)
    end
    em.cursor = Cursor(targetline, col)
end
function cmove_right(em::ScreenEmulator, n)
    em.debug && println("Moving $n right")
    em.cursor = Cursor(em.cursor.line, em.cursor.column+n)
end
function cmove_col(em::ScreenEmulator, n)
    em.debug && println("Moving to col $n")
    em.cursor = Cursor(em.cursor.line, n)
end
function cmove_down(em::ScreenEmulator, n)
    em.debug && println("Moving $n down")
    em.cursor = Cursor(em.cursor.line + n, em.cursor.column)
end

# An emulator that works a line at a time and does not support screen movement
# commands
type LineEmulator <: Emulator
    cur_cell::Cell
    linedrawing::Bool
    debug::Bool
    LineEmulator(cur_cell::Cell) = new(cur_cell,false,false)
end
create_cell(em::LineEmulator,c::Char) = Cell(em.cur_cell, content = c)
write(em::LineEmulator, c) = 1
cur_cell(em::LineEmulator) = em.cur_cell
set_cur_cell(em::LineEmulator,c::Cell) = em.cur_cell = c
insert_line!(em::LineEmulator) = nothing
cmove_down(em::LineEmulator,_) = nothing

# Erase is inclusive the cursor position.
erase_eol(em) = resize!(em.lines[em.cursor.line].data, em.cursor.column-1)
function erase_sol(em)
    for i = 1:em.cursor.column
        em.lines[em.cursor.line].data[i] = Cell(' ')
    end
end
function erase_n(em,n)
    em.debug && println("Erasing $n characters")
    for i = em.cursor.column:(em.cursor.column+(n-1))
        if i < length(em.lines[em.cursor.line].data)
            em.lines[em.cursor.line].data[i] = Cell(' ')
        end
    end
end
erase_line(em,i=em.cursor.line) = (i <= length(em.lines)) && resize!(em.lines[i].data,0)
function erase_bos(em)
    erase_eol(em)
    for i in (em.cursor.line+1):(em.firstline + em.ViewPortSize.height)
        erase_line(em,i)
    end
end
function erase_tos(em)
    for i in (em.firstline):(em.cursor.line-1)
        erase_line(em,i)
    end
    erase(eol)
end
function erase_screen(em)
    for i in (em.firstline):(em.firstline + em.ViewPortSize.height)
        erase_line(em,i)
    end
end

function add_line!(em::Emulator)
    a = Array(Cell, 0)
    sizehint!(a, 80)
    push!(em.lines, a)
end


# Line Drawing Character Set
const LineDrawing = [
    # see http://vt100.net/docs/vt100-ug/table3-9.html
    collect(Char(0x00):Char(0x5e)); # Octal 0 - 136 are regular ASCII characters
    # Actual characters chosen for compatibilty with iterm
    # genrated using write(STDOUT,"\e(0",char(i),"\e(A")
    Char(0x0a);              # Blank Space
    '◆';'▒';'␉';'␌';'␍';'␊';'°';'±';'␤';'␋';
    '┘';'┐';'┌';'└';'┼';'⎺';'⎻';'─';'─';'⎼';
    '├';'┤';'┴';'┬';'│';'≤';'≥';'π';'≠';'£';
    '·';
    collect(Char(0x00):Char(0x5e)); # Octal 0 - 136 are regular ASCII characters
]

# Maintains an array of UTF8 sequences for combining characters, etc. Indexed
# by C-U+F0000
const ExtendedContents = UTF8String[]

# Render the contents of this emulator into another terminal.
function render(term::IO, em::Emulator)
    dump(term,DevNull,em)
end

# Dump the emulator contents as a plain-contents text file and a decorator file
# Intended mostly for regression testing.
function dump(contents::IO, decorator::IO, em::Emulator, lines = nothing)
    first = true
    for line in (lines === nothing ? em.lines : em.lines[lines])
        if first
            first = false
        else
            println(contents)
        end
        for cell in line
            c = cell.content
            if c < '\Uf0000'
                write(contents,c)
            else
                write(contents,em.ExtendedContents[c])
            end
        end
    end
end

function fill_space!(line, from, to)
    resize!(line.data, to)
    for i = (from:to)
        line.data[i] = Cell(' ')
    end
end

function insert_cell!(em, pos, c::Cell)
    if em.cursor.line > length(em.lines)
        add_line!(em, em.cursor.line)
    end
    line = em.lines[em.cursor.line]
    l = length(line.data)
    if pos > em.ViewPortSize.width
        # Fill the remainder of this line with spaces
        fill_space!(line, l+1, em.ViewPortSize.width)
        # Remember that this line was wrapper
        line.wrapped = true
        insert_line!(em)
        cmove_down(em, 1); cmove_col(em, 1)
        return insert_cell!(em, 1, c)
    else
        if pos > l
            fill_space!(line, l+1, pos)
        end
        line.data[pos] = c
    end
end

function fill_lines(em, from, to)
    resize!(em.lines, to)
    for i = (from:to)
        em.lines[i] = Line()
    end
end

function add_line!(em::Emulator, pos)
    a = Array(Cell, 0)
    sizehint!(a, 80)
    l = length(em.lines)
    if pos > l + 1
        fill_lines(em,l+1,pos-1)
    end
    insert!(em.lines, pos, a)
end
insert_line!(em::Emulator) = add_line!(em::Emulator, em.cursor.line + 1)

function write(em::ScreenEmulator, c::Cell)
    insert_cell!(em,em.cursor.column,c)
    cmove_right(em, 1)
end

# Writing characters (non-control)
function write(em::ScreenEmulator, c::Char)
    em.debug && println(c,"Writing ",repr(c))
    write(em,Cell(c))
end

# Parsing
@enum State char color

const decbuf = IOBuffer()
sizehint!(decbuf.data,5)

function readdec(io)
    truncate(decbuf, 0)
    n = 0
    while true
        c = read(io,Char)
        if '0' <= c <= '9'
            write(decbuf,c)
        elseif n == 0
            return (c, -1)
        else
            return (c, parse(Int,takebuf_string(decbuf),10))
        end
        n += 1
    end
end

const colorsInOrder =
    [:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white]
function process_CSIm(em,f1)
    cell = cur_cell(em)
    if f1 == 0
        em.debug && println("Change color to default")
        set_cur_cell(em,Cell(Cell('\0'),fg=9,bg=9))
    elseif f1 == 1
        set_cur_cell(em,Cell(cell,attrs=cell.attrs | Bright))
    elseif 30 <= f1 <= 39
        em.debug && println("Change fg color")
        set_cur_cell(em,Cell(cell,fg = f1-30))
    elseif 40 <= f1 <= 49
        em.debug && println("Change bg color")
        set_cur_cell(em,Cell(cell,bg = f1-40))
    else
        error("Unimplemented")
    end
end

# Parse everything and append it to the emulator
function parseall!(em::Emulator,io::IO)
    while !eof(io::IO)
        parse!(em,io)
    end
end

# Parse until we have gotten a full cell that needs to be printed
function parse_cell!(em::Emulator, io::IO)
    local c
    while !eof(io) && (c = parse!(em,io)).content == 0
    end
    c
end

function parse!(em::Emulator, io::IO)
    const debug = true
    c = read(io, Char)
    if c == '\r'
        cmove_col(em, 1)
    elseif c == '\n'
        insert_line!(em)
        cmove_down(em, 1)
    elseif c == '\e'
        c = read(io, Char)
        if c == '['
            (c,f1) = readdec(io)
            n = f1 == -1 ? 1 : f1
            if c == ';'
                (c,f2) = readdec(io)
                n2 = f2 == -1 ? 1 : f2
                if c == ';'
                    fs = [f1,f2]
                    while c == ';'
                        (c,f) = readdec(io)
                        push!(fs, f)
                    end
                end
                if c == 'H'             # CUP (two arg)
                    cmove(em, n, n2)
                elseif c == 'm'
                    # error("TODO: Color")
                elseif c == 'r'
                    # error("TODO: DECSTBM")
                else
                    error("TODO: $(hex(c))")
                end
            elseif c == '?'
                (c,f2) = readdec(io)
            elseif c == 'A'             # CUU
                cmove_up(em, n)
            elseif c == 'B'             # CUD
                cmove_down(em, n)
            elseif c == 'C'             # CUF
                cmove_right(em, n)
            elseif c == 'D'             # CUB
                cmove_left(em, n)
            elseif c == 'E'             # CNL
                cmove_line_down(em,n)
            elseif c == 'F'             # CPL
                cmove_line_up(em,n)
            elseif c == 'G'             # CHA
                cmove_col(em, n)
            elseif c == 'H'             # CUP (one arg)
                cmove(em, n, 1)
            elseif c == 'K'
                if f1 == -1 || f1 == 0  # \e[K
                    erase_eol(em)
                elseif f1 == 1          # \e[1K
                    erase_sol(em)
                elseif f1 == 2          # \e[2K
                    erase_line(em)
                else
                    error("Unknown erase command")
                end
            elseif c== 'J'
                if f1 == -1 || f1 == 0  # \e[J
                    erase_bos(em)
                elseif f1 == 1          # \e[1J
                    erase_tos(em)
                elseif f1 == 2          # \e[2J
                    erase_screen(em)
                else
                    error("Unknown erase command")
                end
            elseif c == 'S' || c== 'T'
                error("TODO: Scroll")
            elseif c == 'm'
                process_CSIm(em,f1)
            elseif c == 'l'
                # error("TODO: l")
            elseif c == 'X'             # ECH
                erase_n(em,n)
            else
                error("Unimplemented CSI $c")
            end
        elseif c == '('
            c = read(io,Char)
            if c == 'B'
                em.debug && println("Disable linedrawing")
                em.linedrawing = false
            elseif c == '0'
                em.debug && println("Enable linedrawing")
                em.linedrawing = true
            else
                error("Unimplemented \\e($c")
            end
        elseif c == '='
            # DECKPAM
        else
            error("Unhandled escape sequence starting with $c")
        end
    else
        if em.linedrawing && c < 0xff
            cell = create_cell(em,LineDrawing[UInt8(c)+1])
            write(em, cell)
            return cell
        else
            cell = create_cell(em,c)
            write(em, cell)
            return cell
        end
    end
    return Cell('\0')
end

@unix_only begin
    type PTY
        em::Emulator
        master::Base.TTY
        slave::RawFD
    end

    function create_pty(parse = true)
        const O_RDWR = Base.FS.JL_O_RDWR
        const O_NOCTTY = Base.FS.JL_O_NOCTTY

        fdm = ccall(:posix_openpt,Cint,(Cint,),O_RDWR|O_NOCTTY)
        fdm == -1 && error("Failed to open PTY master")
        rc = ccall(:grantpt,Cint,(Cint,),fdm)
        rc != 0 && error("grantpt failed")
        rc = ccall(:unlockpt,Cint,(Cint,),fdm)
        rc != 0 && error("unlockpt")

        fds = ccall(:open,Cint,(Ptr{UInt8},Cint),
            ccall(:ptsname,Ptr{UInt8},(Cint,),fdm), O_RDWR|O_NOCTTY)

        slave  = RawFD(fds)
        master = Base.TTY(RawFD(fdm); readable = true)

        pty = PTY(Emulator(), master, slave)
        parse && @async parseall!(pty.em,master)

        finalizer(pty, close)
        pty
    end

    import Base: close
    close(pty::PTY) = close(pty.master)
end

end # module
