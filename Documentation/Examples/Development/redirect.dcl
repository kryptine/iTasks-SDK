definition module redirect

// call_process_with_redirected_std_out_and_error command directory out_file_name errors_file_name world

call_process_with_redirected_std_out_and_error :: !{#Char} !{#Char} !{#Char} !{#Char} !*World -> (!(!Bool, !Int), !*World);
