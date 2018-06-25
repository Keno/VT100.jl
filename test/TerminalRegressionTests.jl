module TerminalRegressionTests
    using VT100

    function load_outputs(file)
        outputs = String[]
        decorators = String[]
        is_output = true
        is_continuation = false
        nlines = 0
        for line in readlines(file)
            if line[1] == '+'
                is_continuation = line[2] != '+'
                if is_continuation
                  until = findfirst(c -> c == '+', line[2:end])+1
                  nlines = parse(Int, line[2:until - 1])
                  push!(outputs,
                    join(split(outputs[end],'\n')[1:end-nlines],'\n'))
                else
                  push!(outputs,"")
                end
                is_output = true
                continue
            elseif line[1] == '-'
                if is_continuation
                  push!(decorators,
                    join(split(decorators[end],'\n')[1:end-nlines],'\n'))
                else
                  push!(decorators,"")
                end
                is_output = false
                continue
            elseif line[1] == '|'
                array = is_output ? outputs : decorators
                array[end] = string(array[end],isempty(array[end]) ? "" : "\n",line[2:end])
            else
                error("Unrecognized first character \"$(line[1])\"")
            end
        end
        outputs, decorators
    end

    mutable struct EmulatedTerminal <: Base.Terminals.UnixTerminal
        input_buffer::IOBuffer
        out_stream::Base.TTY
        pty::VT100.PTY
        terminal::VT100.ScreenEmulator
        waiting::Bool
        step::Condition
        filled::Condition
        # Yield after every write, e.g. to test for flickering issues
        aggressive_yield::Bool
        function EmulatedTerminal()
            pty = VT100.create_pty(false)
            new(
            IOBuffer(UInt8[], true, true, true, true, typemax(Int)),
            Base.TTY(pty.slave; readable = false), pty,
            pty.em, false, Condition(), Condition())
        end
    end
    function Base.wait(term::EmulatedTerminal)
        if !term.waiting || bytesavailable(term.input_buffer) != 0
            wait(term.step)
        end
    end
    for T in (Vector{UInt8}, Array, AbstractArray, String, Symbol, Any, Char, UInt8)
        function Base.write(term::EmulatedTerminal,a::T)
            write(term.out_stream, a)
            if term.aggressive_yield
                notify(term.step)
            end
        end
    end
    Base.eof(term::EmulatedTerminal) = false
    function Base.read(term::EmulatedTerminal, ::Type{Char})
        if bytesavailable(term.input_buffer) == 0
            term.waiting = true
            notify(term.step)
            wait(term.filled)
        end
        term.waiting = false
        read(term.input_buffer, Char)
    end
    function Base.readuntil(term::EmulatedTerminal, delim::UInt8)
        if bytesavailable(term.input_buffer) == 0
            term.waiting = true
            notify(term.step)
            wait(term.filled)
        end
        term.waiting = false
        readuntil(term.input_buffer, delim)
    end
    Base.Terminals.raw!(t::EmulatedTerminal, raw::Bool) =
        ccall(:jl_tty_set_mode,
                 Int32, (Ptr{Cvoid},Int32),
                 t.out_stream.handle, raw) != -1
    Base.Terminals.pipe_reader(t::EmulatedTerminal) = t.input_buffer
    Base.Terminals.pipe_writer(t::EmulatedTerminal) = t.out_stream

    function _compare(output, outbuf)
        result = outbuf == output
        if !result
            println("Test failed. Expected result written to expected.out,
                actual result written to failed.out")
            open("failed.out","w") do f
                write(f,outbuf)
            end
            open("expected.out","w") do f
                write(f,output)
            end
            for (i,c) in enumerate(output)
                if c == Char(outbuf[i])
                    print(Char(c))
                elseif c == '\n'
                    println()
                else
                    print_with_color(:red, stdout, "█")
                end
            end
            error()
        end
        return result
    end

    function compare(em, output, decorator = nothing)
        buf = IOBuffer()
        decoratorbuf = IOBuffer()
        VT100.dump(buf,decoratorbuf,em)
        outbuf = take!(buf)
        decoratorbuf = take!(decoratorbuf)
        _compare(Vector{UInt8}(output), outbuf) || return false
        decorator === nothing && return true
        _compare(Vector{UInt8}(decorator), decoratorbuf)
    end

    function process_all_buffered(emuterm)
        # Since writes to the tty are asynchronous, there's an
        # inherent race condition between them being sent to the
        # kernel and being available to epoll. We write a sentintel value
        # here and wait for it to be read back.
        sentinel = Ref{UInt32}(0xffffffff)
        ccall(:write, Cvoid, (Cint, Ptr{UInt32}, Csize_t), emuterm.pty.slave, sentinel, sizeof(UInt32))
        Base.process_events(false)
        # Read until we get our sentinel
        while bytesavailable(emuterm.pty.master) < sizeof(UInt32) ||
            reinterpret(UInt32, emuterm.pty.master.buffer.data[(emuterm.pty.master.buffer.size-3):emuterm.pty.master.buffer.size])[] != sentinel[]
            emuterm.aggressive_yield || yield()
            Base.process_events(false)
            sleep(0.01)                        
        end
        data = IOBuffer(readavailable(emuterm.pty.master)[1:(end-4)])
        while bytesavailable(data) > 0
            VT100.parse!(emuterm.terminal, data)
        end
    end

    function automated_test(f, outputpath, inputs; aggressive_yield = false)
        emuterm = EmulatedTerminal()
        emuterm.aggressive_yield = aggressive_yield
        emuterm.terminal.warn = true
        outputs, decorators = load_outputs(outputpath)
        c = Condition()
        @async Base.wait_readnb(emuterm.pty.master, typemax(Int64))
        yield()
        t1 = @async try
            f(emuterm)
            Base.notify(c)
        catch err
            Base.showerror(STDERR, err, catch_backtrace())
            Base.notify_error(c, err)
        end
        t2 = @async try
            for input in inputs
                wait(emuterm);
                emuterm.aggressive_yield || @assert emuterm.waiting
                output = shift!(outputs)
                decorator = isempty(decorators) ? nothing : shift!(decorators)
                @assert !eof(emuterm.pty.master)
                process_all_buffered(emuterm)
                compare(emuterm.terminal, output, decorator)
                print(emuterm.input_buffer, input); notify(emuterm.filled)
            end
            Base.notify(c)
        catch err
            Base.showerror(STDERR, err, catch_backtrace())
            Base.notify_error(c, err)
        end
        while !istaskdone(t1) || !istaskdone(t2)
            wait(c)
        end
    end
    
    function create_automated_test(f, outputpath, inputs; aggressive_yield=false)
        emuterm = EmulatedTerminal()
        emuterm.aggressive_yield = aggressive_yield
        emuterm.terminal.warn = true
        c = Condition()
        @async Base.wait_readnb(emuterm.pty.master, typemax(Int64))
        yield()
        t1 = @async try
            f(emuterm)
            Base.notify(c)
        catch err
            Base.showerror(STDERR, err, catch_backtrace())
            Base.notify_error(c, err)
        end
        t2 = @async try
            outs = map(inputs) do input
                wait(emuterm);
                emuterm.aggressive_yield || @assert emuterm.waiting
                process_all_buffered(emuterm)
                out = IOBuffer()
                decorator = IOBuffer()
                VT100.dump(out, decorator, emuterm.terminal)
                @show String(out)
                @show String(input)
                print(emuterm.input_buffer, input); notify(emuterm.filled)
                out, decorator
            end
            open(outputpath, "w") do io
                print(io,"+"^50,"\n",
                    join(map(outs) do x
                        sprint() do io
                            out, dec = x
                            print(io, "|", replace(String(take!(out)),"\n","\n|"))
                            println(io, "\n", "-"^50)
                            print(io, "|", replace(String(take!(dec)),"\n","\n|"))
                        end
                    end, string('\n',"+"^50,'\n')))
            end
            Base.notify(c)
        catch err
            Base.showerror(STDERR, err, catch_backtrace())
            Base.notify_error(c, err)
        end
        while !istaskdone(t1) || !istaskdone(t2)
            wait(c)
        end
    end
end
