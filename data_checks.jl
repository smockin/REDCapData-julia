date_can_be_validated(var) = begin
    if ismissing(var)
        return false
    end
    if isnull(var)
        return false
    end
    if strip(var) == ""
        return false
    end
    var = strip(var)
    if !is_date(var)
        return false
    end
    if as_date(var) in seq(as_date("1910-01-01"), as_date("1950-01-01"), by = "year")
        return false
    end
    true
end


function data_can_be_validated(var)
    if isa(var, Missing)
        return false
    end
    if isa(var, Nullable{T}) && ismissing(var)
        return false
    end
    if isa(var, String) && (var == "" || var == "-1" || var == "empty")
        return false
    end
    if isa(var, Date)
        return date_can_be_validated(var)
    end
    return true
end


function data_missing(var)
    if ismissing(var)
        return true
    end
    if isempty(var)
        return true
    end
    return false
end
