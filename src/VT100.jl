__precompile__()

module VT100

using ColorTypes
using FixedPointNumbers

const RGB8 = RGB{N0f8}

export ScreenEmulator, LineEmulator, Emulator, parse!, parse_cell!, Cell,
    parseall!
import Base: convert, write
import REPL
import REPL.Terminals: cmove_right
import Base: iterate, setindex!, getindex, lastindex

module Attributes
    export Bright, Dim, Underline, Blink,
        Reverse, Hidden, Italics, IsACS
    const Bright        = 0x1
    const Dim           = 0x2
    const Underline     = 0x4
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
using .Flags

# One terminal cell on the screen. Needs to take care of
# the contents of the screen as well as the coloring.
struct Cell
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

const colorlist = Dict(
    :black      => 0,
    :red        => 1,
    :green      => 2,
    :yellow     => 3,
    :blue       => 4,
    :magenta    => 5,
    :cyan       => 6,
    :white      => 7,
    :default    => 9
)

function Cell(c::Cell;
        content = c.content, flags = c.flags, fg = c.fg, bg = c.bg,
        attrs = c.attrs, fg_rgb = c.fg_rgb, bg_rgb = c.bg_rgb)
    isa(fg, Symbol) && (fg = colorlist[fg]; flags & ~(FG_IS_256 | FG_IS_RGB))
    isa(bg, Symbol) && (bg = colorlist[bg]; flags & ~(BG_IS_256 | BG_IS_RGB))
    Cell(content,flags,fg,bg,attrs,fg_rgb,bg_rgb)
end
Cell(c::Char) = Cell(c,0,0,0,0,RGB8(0,0,0),RGB8(0,0,0))

# Encode x information if foreground color, y information in background color
# r encodes the lowest 8 bits, g the next, b the hight bits
function color_for_int(x)
    @assert (x & ~0xFFFFFF) == 0
    RGB8(
        N0f8(UInt8( x      & 0xFF),nothing),
        N0f8(UInt8((x>> 8) & 0xFF),nothing),
        N0f8(UInt8((x>>16) & 0xFF),nothing)
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

mutable struct Line
    data::Vector{Cell}
    wrapped::Bool
    Line() = new(Vector{Cell}(0),false)
    Line(data::Vector{Cell}) = new(data,false)
end
iterate(l::Line, args...) = iterate(l.data, args...)
setindex!(l::Line, c, i) = setindex!(l.data, c, i)
getindex(l::Line,i) = getindex(l.data,i)
lastindex(l::Line) = lastindex(l.data)

convert(::Type{Line}, data::Vector{Cell}) = Line(data)

struct Cursor
    line::Int
    column::Int
end

struct Size
    width::Int
    height::Int
end

abstract type Emulator end

mutable struct ScreenEmulator <: Emulator
    ViewPortSize::Size
    firstline::Int
    ExtendedContents::Vector{String}
    lines::Vector{Line}
    cursor::Cursor
    cur_cell::Cell
    debug::Bool # Log what we're doing
    warn::Bool # Complain about bad data
    linedrawing::Bool
    function ScreenEmulator(width = 80, height = 24)
        this = new(Size(width, height),1,
            Vector{String}(),Vector{Line}(),Cursor(1,1),Cell('\0'),
            false, false, false)
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
    @assert col >= 1
    em.cursor = Cursor(targetline, col)
end
function cmove_right(em::ScreenEmulator, n)
    em.debug && println("Moving $n right")
    em.cursor = Cursor(em.cursor.line, em.cursor.column+n)
end
function cmove_col(em::ScreenEmulator, n)
    if n == 0
        em.warn && println(stderr, "BAD DATA: Columns are 1 indexed.")
        n = 1
    end
    em.debug && println("Moving to col $n")
    em.cursor = Cursor(em.cursor.line, n)
end
function cmove_down(em::ScreenEmulator, n)
    em.debug && println("Moving $n down")
    em.cursor = Cursor(em.cursor.line + n, em.cursor.column)
end
function cmove_up(em::ScreenEmulator, n)
    em.debug && println("Moving $n down")
    em.cursor = Cursor(em.cursor.line - n, em.cursor.column)
end

# An emulator that works a line at a time and does not support screen movement
# commands
mutable struct LineEmulator <: Emulator
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
    a = Array{Cell}(undef, 0)
    sizehint!(a, 80)
    push!(em.lines, a)
    em.debug && println("Adding line to emulator")
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
const ExtendedContents = String[]

# Render the contents of this emulator into another terminal.
function render(term::IO, em::Emulator)
    dump(term,devnull,em)
end

# Dump the emulator contents as a plain-contents text file and a decorator file
# Intended mostly for regression testing.
const default_decorators = ['A':'z';'a':'z';'0':'9']
function dump(contents::IO, decorator::IO, em::Emulator, lines = nothing, decorator_map = Dict{Cell,Char}(),
        available_decorators = copy(default_decorators))
    first = true
    for line in (lines === nothing ? em.lines : em.lines[lines])
        if first
            first = false
        else
            println(contents)
            println(decorator)
        end
        for cell in line
            c = cell.content
            if c < '\Uf0000'
                write(contents,c)
            else
                write(contents,em.ExtendedContents[c])
            end
            # Write decorator
            template = Cell(cell,content='\0')
            if !haskey(decorator_map, template)
                decorator_char = popfirst!(available_decorators)
                decorator_map[template] = decorator_char
            end
            write(decorator, decorator_map[template])
        end
    end
end

function fill_space!(line, from, to)
    @assert to >= 0
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
    @assert to >= 0
    resize!(em.lines, to)
    for i = (from:to)
        em.lines[i] = Line()
    end
end

function add_line!(em::Emulator, pos)
    a = Array{Cell}(undef, 0)
    sizehint!(a, 80)
    l = length(em.lines)
    if pos > l + 1
        em.debug && println("Inserting $(pos-1-(l+1)) lines")
        fill_lines(em,l+1,pos-1)
    end
    em.debug && println("Inserting line (now $(length(em.lines)) lines)")
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
            return (c, parse(Int, String(take!(decbuf)), base=10))
        end
        n += 1
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
    c = Cell('\0')
    while !eof(io) && (c = parse!(em,io)).content == Char(0)
    end
    c
end

function parseSGR!(em::Emulator, params)
    idx = 1
    while idx <= length(params)
        cell = cur_cell(em)
        f1 = params[idx]
        if f1 == 0
            em.debug && println("Change color to default")
            set_cur_cell(em,Cell(Cell('\0'),fg=9,bg=9))
        elseif f1 == 1
            set_cur_cell(em,Cell(cell,attrs=cell.attrs | Bright))
        elseif f1 == 4
            set_cur_cell(em,Cell(cell,attrs=cell.attrs | Underline))
        elseif f1 == 22
            set_cur_cell(em,Cell(cell,attrs=cell.attrs & ~(Bright | Dim)))
        elseif 30 <= f1 <= 37 || f1 == 39
            em.debug && println("Change fg color")
            set_cur_cell(em,Cell(cell,fg = f1-30))
        elseif f1 == 38
            idx += 2
            f2, f3 = params[(idx-1):idx]
            if f2 == 2
                idx += 2
                f4, f5 = params[(idx-1):idx]
                set_cur_cell(em,Cell(cell, fg_rgb = RGB8(N0f8(f3,0), N0f8(f4,0), N0f8(f5,0)), flags=cell.flags | FG_IS_RGB))
            elseif f2 == 5
                set_cur_cell(em,Cell(cell, fg = f3, flags=cell.flags | FG_IS_256))
            else
                error("Incorrect SGR sequence")
            end
        elseif 40 <= f1 <= 47 || f1 == 49
            em.debug && println("Change bg color")
            set_cur_cell(em,Cell(cell,bg = f1-40))
        elseif f1 == 48
            idx += 2
            f2, f3 = params[(idx-1):idx]
            if f2 == 2
                idx += 2
                f4, f5 = params[(idx-1):idx]
                set_cur_cell(em,Cell(cell, bg_rgb = RGB8(N0f8(f3,0), N0f8(f4,0), N0f8(f5,0)), flags=cell.flags | BG_IS_RGB))
            elseif f2 == 5
                set_cur_cell(em,Cell(cell, bg = f3, flags=cell.flags | BG_IS_256))
            else
                error("Incorrect SGR sequence")
            end
        elseif 90 <= f1 <= 97
            set_cur_cell(em,Cell(cell,fg = f1-90, attrs=cell.attrs | Bright))
        elseif 100 <= f1 <= 107
            set_cur_cell(em,Cell(cell,fg = f1-100, attrs=cell.attrs | Bright))
        else
            error("Unimplemented CSIm $f1")
        end
        idx += 1
    end
end

function parse!(em::Emulator, io::IO)
    debug = true
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
                fm = [f1]
                while c == ';'
                    (c,fs) = readdec(io)
                    push!(fm, fs)
                end
                n2 = fm[2] == -1 ? 1 : fm[2]
                if c == 'H'             # CUP (two arg)
                    cmove(em, n, n2)
                elseif c == 'm'
                    parseSGR!(em, fm)
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
                parseSGR!(em,(f1,))
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


@static if Sys.isunix()
    mutable struct PTY
        em::ScreenEmulator
        master::Base.TTY
        slave::RawFD
        fdm::RawFD
    end

    const O_RDWR = Base.Filesystem.JL_O_RDWR
    const O_NOCTTY = Base.Filesystem.JL_O_NOCTTY
    function create_pty(parse = true)
        fdm = ccall(:posix_openpt,Cint,(Cint,),O_RDWR|O_NOCTTY)
        fdm == -1 && error("Failed to open PTY master")
        rc = ccall(:grantpt,Cint,(Cint,),fdm)
        rc != 0 && error("grantpt failed")
        rc = ccall(:unlockpt,Cint,(Cint,),fdm)
        rc != 0 && error("unlockpt")

        fds = ccall(:open,Cint,(Ptr{UInt8},Cint),
            ccall(:ptsname,Ptr{UInt8},(Cint,),fdm), O_RDWR|O_NOCTTY)

        slave  = RawFD(fds)
        master = Base.TTY(RawFD(fdm))

        pty = PTY(ScreenEmulator(), master, slave, RawFD(fdm))
        parse && @async parseall!(pty.em,master)

        finalizer(close, pty)
        pty
    end

    import Base: close
    close(pty::PTY) = close(pty.master)
end

end # module
