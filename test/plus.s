
section .text
global our_code_starts_here
our_code_starts_here:
    
    
    mov rax, 3
    mov [rsp - 8], rax
    mov rax, 4
    add rax, [rsp - 8]
                
add rax , 1
    mov [rsp - 8], rax
    mov rax, 6
    add rax, [rsp - 8]
                
    ret
