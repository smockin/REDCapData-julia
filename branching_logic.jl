function remove_html_tags(x)
    reduce(vcat, map(x) do s
        gsub(r"<[^>]*>", "", s)
    end)
end

function logical_xpressions_red2r(x)
    pattern_match = regexmatch(r"[ \t]*((==)|(!=))[ \t]*'[\\-]*[a-zA-Z0-9]*'", x)
    if pattern_match != nothing
        len_match = length(pattern_match) + pattern_match
        start_part = x[1:pattern_match - 1]
        var_part = regexmatch(r"[ \t]*[\\-]*[a-zA-Z0-9_]+[ \t]*", start_part)
        len_part = length(var_part)
        len_part = len_part[end]
        var_part = var_part[end]
        var_part = strip(x[var_part:len_part + var_part + 1])
        match_part = x[pattern_match:len_match - 1]
        end_part = x[len_match:end]
        has_value = regexmatch(r"'[\\-]*[a-zA-Z0-9]+'", match_part)
        has_value_len = length(has_value)
        value_2_chk = match_part[has_value + 1:has_value + has_value_len - 2]
        if has_value_len > 2
            if is_number(value_2_chk)
                match_part = replace(match_part, r"'", "")
            else
                match_part = replace(match_part, r"'", "\"")
            end
        else
            test_eq = occursin(r"==", match_part)
            if test_eq
                match_part = "(any(is.na($var_part), str_trim($var_part) == \"\"))"
            else
                match_part = "(all(!is.na($var_part), str_trim($var_part) != \"\"))"
            end
            start_part = x[1:end - len_part]
        end
        x = "$start_part$match_part$end_part"
        if regexmatch(r"[ \t]*((==)|(!=))[ \t]*'[\\-]*[a-zA-Z0-9]*'", x) != nothing
            x = logical_xpressions_red2r(x)
        end
        x
    end
    x
end


function convert_missing_red2r(x)
    reshape_na_red2r(x) = begin
        ._i = parse(Int, regexp(lowercase(x), "[a-z]"))
        start_part = x[1:._i-1]
        name_part = x[._i:end]
        ._i = parse(Int, regexp(name_part, "[^a-zA-Z0-9_]") - 1)
        name_part = name_part[1:._i]
        if regexp(name_part, "!=") > 0
            cmd = "(!any(is.na($name_part), $name_part == \"\"))"
        else
            cmd = "(any(is.na($name_part), $name_part == \"\"))"
        end
        "$start_part$cmd"
    end
    string_xtract = split(x, "'")
    idx = 1:length(string_xtract)
    idx = idx[idx .% 2 .== 0]
    string_xtract_chkd = string_xtract[idx]
    string_xtract_chkd = map(string_xtract_chkd) do x
        if x == ""
            value = NA
        else
            value = parse(Float64, x)
            if isna(value)
                value = "\"$x\""
            else
                value = x
            end
        end
        value
    end
    if any(isna.(string_xtract_chkd))
        idx_change = findall(isna.(string_xtract_chkd))
        idx_change = (2 .* idx_change) .- 1
        string_xtract[idx_change] = map(string_xtract[idx_change]) do x
            reshape_na_red2r(x)
        end
    end
    string_xtract[idx] = string_xtract_chkd
    if any(isna.(string_xtract))
        string_xtract = string_xtract[findall(!isna.(string_xtract))]
    end
    string_xtract = join(string_xtract)
    string_xtract
end

function xtend_chb_names(x)
    pad = "___"
    pattern_match = match(r"\([0-9]{1,}\)", x)
    pattern_start_pos = pattern_match .+ 1
    pattern_start_pos_bckp = pattern_match
    pattern_len = length(pattern_match) .- 3
    pattern_len = pattern_len .+ pattern_start_pos
    pattern_len_bckp = pattern_start_pos_bckp .+ length(pattern_match)
    x = repeat(x, length(pattern_start_pos))
    replace_numeric_match(a, b, c) = begin
        value = a[b:c]
        if !isna(parse(Int, value))
            value = pad * value
            a[pattern_start_pos_bckp:pattern_len_bckp] = value
        end
        a
    end
    mapply(replace_numeric_match, x, pattern_start_pos, pattern_len)
end


function convert_space2tab(x)
    if regexmatch(r"^[ \t]+", x) > 0
        stop_val = length(match(r"^[ \t]+", x))
        substr(x, 1, stop_val) = repeat("\t", stop_val)
    end
    x
end



function convert_dates_red2r(x)
    pattern_match = match(r"\'[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}\'", x)
    if pattern_match > 0
        tmp = x[pattern_match:pattern_match+11]
        newVal = "as.Date(\"" * tmp[2:end-1] * "\")"
        start_part = x[1:pattern_match-1]
        end_part = x[pattern_match+12:end]
        x = start_part * newVal * end_part
    end
    if match(r"\'[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}\'", x) > 0
        x = convert_dates_red2r(x)
    end
    x
end


function toproper(x, all = false)
    to_proper_case(w) = begin
        if length(w) != 1
            error("Only one word at at time!")
        end
        w = string(w)
        first = uppercase(w[1])
        last = lowercase(w[2:end])
        string(first, last)
    end
    if ismissing(x)
        return(missing)
    end
    if !all
        x = to_proper_case(x)
    else
        x = mapreduce(to_proper_case, vcat, split(x, " "))
        x = join(x, " ")
    end
    x
end


function convert_redcap2julia(x)
    if !isna(x)
        x = convert_dates_red2r(x)
        x = replace(x, r"\[|\]" => "")
        x = replace(x, r"[ \t]+((AND)|(and))[ \t]+" => " & ")
        x = replace(x, r"[ \t]+((OR)|(or))[ \t]+" => " | ")
        x = replace(x, r"={1}[ \t]*'" => " == '")
        x = replace(x, r"(<>)[ \t]*'" => " != '")
        x = logical_xpressions_red2r(xtend_chb_names(x))
        x = replace(x, r"[ \t]{2, }" => " ")
    end
    x
end
