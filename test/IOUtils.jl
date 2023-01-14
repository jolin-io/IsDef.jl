module IOUtils
export redirect_stdoutio, redirect_stderrio

# taken from https://github.com/JuliaLang/julia/issues/12711#issuecomment-912740865
function redirect_stdoutio(f::Function, io::IO)
    old_stdout = stdout
    rd, = redirect_stdout()
    task = @async write(io, rd)
    try
        ret = f()
        Libc.flush_cstdio()
        flush(stdout)
        return ret
    finally
        close(rd)
        redirect_stdout(old_stdout)
        wait(task)
    end
end

# taken from https://github.com/JuliaLang/julia/issues/12711#issuecomment-912740865
function redirect_stderrio(f::Function, io::IO)
    old_stderr = stderr
    rd, = redirect_stderr()
    task = @async write(io, rd)
    try
        ret = f()
        Libc.flush_cstdio()
        flush(stderr)
        return ret
    finally
        close(rd)
        redirect_stderr(old_stderr)
        wait(task)
    end
end


end
