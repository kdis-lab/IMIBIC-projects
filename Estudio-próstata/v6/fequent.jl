using CSV
using DataFrames

alg= "rf"

cd("f:\\DATA\\backup-29-03-2019\\workspace\\IMIBIC-projects\\Estudio-próstata\\v6\\results\\results-withFW\\$alg")

df= CSV.read("$alg.csv")

factors= ["SNRNP200", "SRRM1", "SRSF3"]

countGloblal=0

for i ∈ eachrow(df)
    st= String(i.Atts)
    ocurrence = occursin.(factors, st)

    if(sum(ocurrence) == length(factors))
        println(st)
        println(i.ROC, " ", i.Sens, " ", i.Spec)
        global countGloblal +=1
    end
end

println(countGloblal)
