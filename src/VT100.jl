module VT100

export Emulator, parse!
import Base: convert, write
import Base.Terminals: cmove_right
import Base: start, next, done

# One terminal cell on the screen. Needs to take care of
# the contents of the screen as well as the coloring.
immutable Cell
    # The content to be drawn in this Cell. Unfortunately a terminal emulator
    # also needs to take care of unicode combining characters, which do not fit
    # in a Char. We assign those characters to one of the private use planes
    # (PUP15) and maintain a separate Array of sequences to be filled in when
    # encountering one of these.
    content::Char

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

convert(::Type{Line}, data::Vector{Cell}) = Line(data)

immutable Cursor
    line::Int
    column::Int
end

immutable Size
    width::Int
    height::Int
end

type Emulator
    ViewPortSize::Size
    firstline::Int
    ExtendedContents::Vector{UTF8String}
    lines::Vector{Line}
    cursor::Cursor
    linedrawing::Bool
    function Emulator(width = 80, height = 24)
        this = new(Size(width, height),1,
            Vector{UTF8String}(0),Vector{Line}(0),Cursor(1,1))
        add_line!(this)
        this
    end
end
cmove(e::Emulator, line, col) =
    e.cursor = Cursor(e.firstline-1+line, col)
cmove_right(e::Emulator, n) =
    e.cursor = Cursor(e.cursor.line, e.cursor.column+n)
cmove_col(e::Emulator, n) =
    e.cursor = Cursor(e.cursor.line, n)
cmove_down(e::Emulator, n) =
    e.cursor = Cursor(e.cursor.line + n, e.cursor.column)

# Erase is inclusive the cursor position.
erase_eol(em) = resize!(em.lines[em.cursor.line].data, em.cursor.column-1)
function erase_sol(em)
    for i = 1:em.cursor.column
        em.lines[em.cursor.line].data[i] = Cell(' ')
    end
end
function erase_n(em,n)
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
    [Char(0x00):Char(0x5e)]; # Octal 0 - 136 are regular ASCII characters
    # Actual characters chosen for compatibilty with iterm
    # genrated using write(STDOUT,"\e(0",char(i),"\e(A")
    Char(0x0a);              # Blank Space
    '◆';'▒';'␉';'␌';'␍';'␊';'°';'±';'␤';'␋';
    '┘';'┐';'┌';'└';'┼';'⎺';'⎻';'─';'─';'⎼';
    '├';'┤';'┴';'┬';'│';'≤';'≥';'π';'≠';'£';
    '·';
    [Char(0x00):Char(0x5e)]; # Octal 0 - 136 are regular ASCII characters
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
            if c < 0xF0000
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

function write(em::Emulator, c::Cell)
    insert_cell!(em,em.cursor.column,c)
    cmove_right(em, 1)
end

# Writing characters (non-control)
function write(em::Emulator, c::Char)
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
            return (c, parseint(takebuf_string(decbuf),10))
        end
        n += 1
    end
end

function parse!(em::Emulator, io::IO)
    while !eof(io)
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
                    # error("TODO: Color")
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
                    em.linedrawing = false
                elseif c == '0'
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
                write(em, LineDrawing[UInt8(c)+1])
            else
                write(em, c)
            end
        end
    end
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
        parse && @async parse!(pty.em,master)

        finalizer(pty, close)
        pty
    end

    import Base: close
    close(pty::PTY) = close(pty.master)
end

end # module
