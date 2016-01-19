using VT100
using Base.Test

for test in [
    "basic/hello", "basic/hello-nocr", "basic/long",
    "basic/color", "basic/boserase"
]
    em = ScreenEmulator(80,24)
    basepath = joinpath(dirname(@__FILE__),test)
    if isfile(string(basepath,".raw.in"))
        open(f->parse!(em,f),string(basepath,".raw.in"))
    else
        @assert isfile(string(basepath,".in"))
        open(f->parse!(em,IOBuffer(unescape_string(readall(f)))),
            string(basepath,".in"))
    end
    output = open(readbytes,string(basepath,".out"))
    buf = IOBuffer()
    VT100.dump(buf,DevNull,em)
    outbuf = takebuf_array(buf)
    if outbuf != output
        println(output)
        println(outbuf)
        println(UTF8String(output))
        println(UTF8String(outbuf))
    end
end
