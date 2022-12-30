function format_branching_logics(metadata::DataFrame, data::DataFrame)
    if !("branching_logic" in names(metadata))
        error("Metadata has no`branching_logic`!")
    end
    records = copy(data)
    metadata[!, :f_branching_logic] = NA_character_
    metadata[!, :f_branching_logic] = apply(metadata, 1, function(br) get_evaluated_branching_logics(br))
    return metadata
end


get_evaluated_branching_logics(br::String) = begin
    toformat = br |>
        replace("and\\[", " & ") |>
        replace("and[ \t]+\\[", " & ") |>
        replace("\\[|\\]", "") |>
        replace("[ \t]and[ \t]|[ \t]+((AND)|(and))[ \t]+", " & ") |>
        replace("[ \t]or[ \t]|[ \t]+((OR)|(or))[ \t]+", " | ") |>
        replace("={1}[ \t]*", " == ")
    toformat = strip(replace(toformat, "<>|(<>)[ \t]", " != ")) |>
        replace("! == ", " != ") |>
        replace("> == ", " >= ") |>
        replace("< == ", " <= ") |>
        replace('"', "'")
    
    if occursin("\\([0-9]+\\)", toformat)
        if occursin("&|\\|", toformat)
            toformat2 = split(toformat, "&|\\|") |>
                map(strip) |>
                collect(Vector{String})
            and_sep = occursin("\\&", toformat) & !occursin("\\|", toformat)
            or_sep = occursin("\\|", toformat) & !occursin("\\&", toformat)
            or_and_sep = occursin("\\|", toformat) & occursin("\\&", toformat)
            
            idx_ = findall(r"\\([0-9]+\\)", toformat2)
            idx_not = findall(r"\\([0-9]+\\)", toformat2)
            sepToUse = ifelse(and_sep, " & ",  ' | ' )
            if length(idx_) == 1
                tfmt = toformat2[idx_]
                toAppend = expand_other_widgets(toformat =tfmt )
                if !isempty(idx_not)
                    if length(idx_not) > 1
                        toformat = join([toformat2[idx_not]; sepToUse; toAppend], sepToUse)
                    else
                        toformat = join([toformat2[idx_not]; sepToUse; toAppend], sepToUse)
                    end
                else
                    toformat = toAppend
                end
            else
                tocombine = map(toformat2) do xx
                    expand_other_widgets(toformat =xx )
                end
                if !or_and_sep
                    combined = join(tocombine, sepToUse)
                else
                    combined = join(tocombine, " & ")
                end
                if isempty(idx_not)
                    toformat = combined 
                else
                    toformat = join([toformat2[idx_not]; combined], "")
                end
            end
        else
            toformat = expand_other_widgets(toformat)
        end
    end
    return(toformat)
end


expand_other_widgets(toformat::String) = begin
    br = Main.br
    records = Main.records
    txt = str_trim(unlist(str_split(toformat, " ")))
    if any(
        all(isna.(txt)) ||
        length(txt) <= 1
    )
        push!(Main.issues, br)
    end
    txt = txt[txt .!= '']
    toformat = txt[1]
    xtract = str_extract(toformat, "\\([0-9]+\\)$")
    level_n = gsub("\\(|\\)", "", xtract)
    xx = substring(toformat
                   , first = 1,
                   last = nchar(toformat) - nchar(xtract))

    txt_f = grep(level_n
                 , grep(xx,
                        names(records),
                        v = true), v = true)
    if length(txt_f) > 1
        underscores = (str_extract_all(txt_f
                                       , regex("[_]{2,}+"
                                       ), true)) |>
            unlist |>
            table |>
            names |>
            x -> x[1]

        toformat = string(xx
                          , underscores
                          , level_n
                          , txt[2]
                          , txt[3]
        )

    else
        toformat = string(txt_f
                          , txt[2]
                          , txt[3]
        )

    end
    return toformat
end
    
