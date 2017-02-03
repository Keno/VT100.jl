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
                array[end] = string(array[end],isempty(array[end])?"":"\n",line[2:end])
            else
                error("Unrecognized first character \"$(line[1])\"")
            end
        end
        outputs, decorators
    end
    
    type EmulatedTerminal <: Base.Terminals.UnixTerminal
        input_buffer::IOBuffer
        out_stream::Base.TTY
        pty::VT100.PTY
        terminal::VT100.ScreenEmulator
        waiting::Bool
        step::Condition
        filled::Condition
        function EmulatedTerminal()
            pty = VT100.create_pty(false)
            new(
            IOBuffer(UInt8[], true, true, true, true, typemax(Int)),
            Base.TTY(pty.slave; readable = false), pty,
            pty.em, false, Condition(), Condition())
        end
    end
    function Base.wait(term::EmulatedTerminal)
        if !term.waiting || nb_available(term.input_buffer) != 0
            wait(term.step)
        end
    end
    for T in (Vector{UInt8}, Array, AbstractArray, String, Any, Char)
        Base.write(term::EmulatedTerminal,a::T) = write(term.out_stream, a)
    end
    Base.eof(term::EmulatedTerminal) = false
    function Base.read(term::EmulatedTerminal, ::Type{Char})
        if nb_available(term.input_buffer) == 0
            term.waiting = true
            notify(term.step)
            wait(term.filled)
        end
        term.waiting = false
        read(term.input_buffer, Char)
    end
    Base.Terminals.raw!(t::EmulatedTerminal, raw::Bool) =
        ccall(:jl_tty_set_mode,
                 Int32, (Ptr{Void},Int32),
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
                    print_with_color(:red, STDOUT, "█")
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
        _compare(decorator, decoratorbuf)
    end
    
    function automated_test(f, outputpath, inputs)
        emuterm = EmulatedTerminal()
        emuterm.terminal.warn = true
        outputs, decorators = load_outputs(outputpath)
        t1 = @async f(emuterm)
        t2 = @async try
                for input in inputs
                    wait(emuterm);
                    @assert emuterm.waiting
                    output = shift!(outputs)
                    decorator = isempty(decorators) ? nothing : shift!(decorators)
                    @assert !eof(emuterm.pty.master)
                    Base.process_events(false)
                    while nb_available(emuterm.pty.master) > 0
                        VT100.parse!(emuterm.terminal, emuterm.pty.master)
                    end
                    compare(emuterm.terminal, output, decorator)
                    print(emuterm.input_buffer, input); notify(emuterm.filled)
                end
            catch err
                Base.showerror(STDERR, err, catch_backtrace())
                rethrow(err)
            end
            
        wait(t2); wait(t1);
    end
end
