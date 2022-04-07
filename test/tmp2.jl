module TestGen
using IRTools: @dynamo

func() = 4

function revise()
    TestGen.@eval begin
        @dynamo function generated_func()
            Expr(:call, identity, func())
        end        
    end
end

revise()

end


TestGen.func()
TestGen.generated_func()
TestGen.func() = 5
TestGen.func()

TestGen.generated_func()
TestGen.revise()
TestGen.generated_func()


struct VAL{Type, Value}
    VAL(Value) = new{typeof(Value), Value}()
end

Base.get(::VAL{Type, Value}) where {Type, Value} = Value


VAL{1}
f() = VAL(1)

Base.Val