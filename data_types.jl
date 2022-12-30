function is_date(x::String)
    if ismissing(x) || x == ""
        return true
    end
    return ismatch(r"^[0-9]{4}[/\\-]{1}[0-9]{2}[/\\-]{1}[0-9]{2}$", x)
end
  
function is_int(x)
    x = string(x)
    if ismissing(x) || x == ""
        return true
    end
    obj = istrue(occursin(r"^[\-]?[0-9]*$", strip(x)) > 0)
    if !obj
        obj = istrue(occursin(r"^[\-]?[0-9]*[\.]?[0-9]?e\+[0-9]*$", strip(x)) > 0)
    end
    return obj
end
  
function is_number(x)
    x = string(x)
    if ismissing(x) || strip(x) == ""
        return true
    end
    if isinteger(x)
        return true
    end
    obj = istrue(regexpr("^[\\-]?[0-9]*[\\.]?[0-9]*$", strip(x)) > 0)
    if !obj
        obj = istrue(regexpr('^[\\-]?[0-9]*[\\.]?[0-9]?e\\+[0-9]*$', strip(x)) > 0)
    end
    return obj
end  


function is_boolean(x)
    x = string(x)
    if ismissing(x) || x == ""
        return true
    end
    x in ["T", "F", "TRUE", "FALSE"]
end


function is_checkbox(varName, metadataName)
    meta = try(DataFrame(get(metadataName)), silent=true)
    if typeof(meta) == "try-error"
        meta = DataFrame(metadataName)
    end
    if !(all(["field_name", "field_type"] .∈ names(meta)))
        error("Metadata must have `field_name` and `field_type`")
    end
    varName = string(varName)
    if istrue(meta[meta.field_name .== varName, :field_type] .== "checkbox")
        return true
    else
        return false
    end
end
