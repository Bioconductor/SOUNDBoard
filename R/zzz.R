.onLoad <-
    function(...)
{
    username <- Sys.getenv("SOUNDBOARD_SERVER_HOST", "soundboard")
    server_path <- file.path("/home", username, "srv")

    SOUNDBoardOption(
        server_host = Sys.getenv("SOUNDBOARD_SERVER_HOST", "localhost"),
        server_path = Sys.getenv("SOUNDBOARD_SERVER_PATH", server_path),
        username = username
    )
}        
