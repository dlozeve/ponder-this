# https://www.research.ibm.com/haifa/ponderthis/challenges/March2021.html

using DelimitedFiles
using JuMP, GLPK

function readgrid(filename)
    open(filename) do f
        parse.(Int, readdlm(f, String); base=16) .- 128
    end
end

function optimalset(grid)
    n = size(grid, 1)
    model = Model(GLPK.Optimizer)
    @variable(model, x[1:n, 1:n], Bin)
    for i = 2:n, j = 1:n
        if j > 1
            @constraint(model, x[i, j] <= x[i-1, j-1])
        end
        @constraint(model, x[i, j] <= x[i-1, j])
        if j < n
            @constraint(model, x[i, j] <= x[i-1, j+1])
        end
    end
    @objective(model, Max, sum(grid[i, j] * x[i, j] for i = 1:n, j = 1:n))

    optimize!(model)
    obj = Int(objective_value(model))
    indices = Tuple.(findall(value.(x) .> 0))
    indices = sort([(a-1, b-1) for (a, b) = indices])
    obj, indices
end

if !isinteractive()
    for filename = ["test.txt", "grid.txt"]
        grid = readgrid(filename)
        obj, indices = optimalset(grid)
        println(obj)
        println(indices)
    end
end
