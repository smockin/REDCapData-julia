function open_using_default_app(file_path)
    if !isfile(file_path)
        error("The file does not exist!")
    end
    if Sys.info()["sysname"] == "Windows"
        run(`cmd /c start $file_path`)
    elseif Sys.info()["sysname"] == "Linux"
        try
            run(`gio open $file_path`)
        catch e
            error(e)
        end
    else
        try
            run(`open $file_path`)
        catch e
            error(e)
        end
    end
    sleep(2)
end
