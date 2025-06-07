


library(sodium)


secret_key <- function(
    config_path = file.path(here::here(), '.VibeCodeR' ),
    key_file = file.path(config_path, '.key_file.key'), 
    ...
  ){
    if ( ! file.exists(key_file)) {
      key <- sodium::keygen()
      writeBin(key, key_file)
      #Sys.chmod(key_file, mode = "600")  # Ensure only the user can read/write
    } else {
      key <- readBin(key_file, what = raw(), n = 32)
    }
    
    key
}


# file_name = config_path |>  file.path('.text_secret_api_key.config')
write_secret_file <- function(
    file_name, 
    secret_text,
    ...,
    key = secret_key(),
    n_byte_nonce = 24
  ){
  
  nonce <- sodium::random( n= n_byte_nonce)
  ciphertext <- sodium::data_encrypt(charToRaw(secret_text), key, nonce)
  
  # Step 3: Save nonce and ciphertext to a file
  saveRDS(list(nonce = nonce, ciphertext = ciphertext), file_name)
}

read_secret_file <- function(
    file_name, 
    ...,
    key = secret_key(...)
  ){
  
  encrypted_data <- readRDS(file = file_name)


  sodium::data_decrypt(
      bin = encrypted_data$ciphertext, 
      key = secret_key(), 
      nonce = encrypted_data$nonce
  ) |> 
  rawToChar()
}



read_vibe_coder_config_file <- function(
          file_name, 
          ...,
          pattern = '_secret_',
          default = ''
    ){
    if ( ! file.exists(file_name) ) {
      return(default)
    }
  
  
   if (grepl(pattern, file_name)){
     read_secret_file(file_name, ...) 
   }else{
     readLines(file_name) 
   } |> paste0(collapse = '\n')
}

write_vibe_coder_config_file <- function(
    file_name, 
    values, 
    ...,
    pattern = '.*_secret_.*'
){
  if (nchar(values) == 0 ){
    return(NULL)
  }
  
  if (grepl(pattern, file_name)){
    write_secret_file(file_name = file_name, secret_text = values, ...)
    #read_secret_file(file_name = file_name)
  }else{
    writeLines(text = values, con = file_name)
  }
}


read_vibe_coder_config <- function(
    fn, 
    config_path = here::here() |> file.path('.VibeCodeR')
    ){
  #file_name <- 
  config_path |> file.path(fn) |>
    read_vibe_coder_config_file()
}
  
#read_vibe_coder_config(fn =  '.default_llm_service.config')
