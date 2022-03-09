.model tiny
.code
.186
org 100h

VIDEOSEG        equ 0B800h
COLOUR          equ 2Bh

Start: jmp main
locals @@

;--------------------------------------------------------------------
New_int09h  proc

Open        db 17h          ; I
Close       db 24h          ; J

            push ax

@@waiting:
            in al, 60h
            cmp al, Open
            jne @@waiting

            call draw_frame

            in al, 61h
            mov ah, al

            or al, 80h
            out 61h, al

            mov al, ah
            out 61h, al

            mov al, 20h
            out 20h, al   

            pop ax
            jmp cs:Old_int09h

New_int09h  endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;itoa:
;   Input: stackг8г
;   Output: AX
;   Registers that change values:
;       BX - offset of the string
;       CX - is used to put values from one part of RAM to another
;       DX - constains the remainder of division after using mul ()
;           than contains offset of the string
;       DI - constains the remainder of division
;--------------------------------------------------------------------
itoa        proc

num         equ [bp + 6]
str         equ [bp + 4]
EOL         db '$'
minus       db '-'

            push bp
            mov bp, sp

            mov ax, num
            mov bx, str
            mov di, 0010h

            cmp ax, 0000h
            je  @@zero

            cmp ax, 0000h
            jb  @@negative

            jmp @@positive

@@zero:     mov ch, numbers[0]
            mov [bx], ch
            inc bx

            mov ch, EOL
            mov [bx], ch
            jmp @@return

@@negative: 
            mov ch, minus
            mov [bx], ch
            inc bx

            neg ax
@@positive: 
            xor dx, dx
            div di
            mov si, dx
            mov ch, numbers[si]
            mov [bx], ch
            xor si, si
            inc bx
            cmp ax, 0000h
            jne @@positive
            
            mov ch, EOL
            mov [bx], ch

            cmp bx, str
            jbe @@return

            mov di, str
@@change:
            dec bx
            mov ch, [bx]

            mov ah, [di]
            mov [bx], ah

            mov [di], ch
            inc di

            cmp bx, di
            ja @@change
        
@@return:
            mov ax, str
            pop bp
            ret

itoa        endp

;--------------------------------------------------------------------
;strlen:
;    Input: stack
;    Output: AX
;    Registers that change values:
;       DI - offset of the string
;       AL - contains end of line symbol
;--------------------------------------------------------------------
strlen      proc

            push bp
            mov bp, sp

            mov al, '$'
            mov di, [bp + 4]
            mov bx, di
            dec di
@@while:
            inc di
            cmp [di], al
            jne @@while

            sub di, bx
            mov ax, di

            pop bp
            ret

strlen      endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
New_Line	proc

            push ax
            push cx
            push dx
            
            xor dx, dx
			mov ax, di
			mov cx, 0A0h
			div cx
			inc ax
			mul cx
			mov di, ax
			mov ah, COLOUR

            pop dx
            pop cx
            pop ax

			ret

New_Line	endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
draw_tb_lines   proc

                lodsb
                stosw

                mov cx, 0009h
                lodsb
@@next_symb:
			    stosw
			    loop @@next_symb

                lodsb
                stosw

                call New_Line

                ret

draw_tb_lines   endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
draw_middle_line    proc

                    push bp
                    mov bp, sp

                    mov cx, 0004h
            @@before_value:  
                    lodsb
                    stosw
                    loop @@before_value

                    push di
                    
                    mov bx, [bp + 4]
                    mov ax, [bx]
                    push ax
                    push offset num_string
                    call itoa
                    pop bx
                    pop bx

                    mov si, ax
                    
                    push si
                    call strlen
                    pop bx

                    pop di

                    cmp ax, 0004h
                    je @@4_digits
                    
                    mov cx, 0004h
                    sub cx, ax
                    mov al, '0'
                    mov ah, COLOUR
                @@fill_spaces:
                    stosw
                    loop @@fill_spaces

                @@4_digits:
                    mov ah, COLOUR
            @@value:    
                    lodsb
                    cmp al, EOL
                    je @@hex_ind
                    stosw
                    jmp @@value

            @@hex_ind:
                    mov al, 'h'
                    mov es:[di], ax
                    add di, 2

                    pop bp
                    ret

draw_middle_line   endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
draw_frame      proc
                
                mov reg_vals[0], ax
                mov reg_vals[2], bx
                mov reg_vals[4], cx
                mov reg_vals[6], dx
                mov reg_vals[8], si
                mov reg_vals[10], di
                mov reg_vals[12], bp
                mov reg_vals[14], sp

                mov ax, VIDEOSEG
                mov es, ax
                mov ah, COLOUR

                xor di, di

                mov si, offset frame
                call draw_tb_lines

                mov si, offset frame + 3

                xor cx, cx
                mov bx, offset reg_vals
            @@middle_lines:
                mov al, [si]
                stosw
                push si

                mov si, offset regs
                add si, cx

                push cx

                push bx
                call draw_middle_line
                pop bx
                add bx, 2

                pop cx

                pop si
                mov al, [si]
                stosw

                call New_Line

                add cx, 0004h
                cmp cx, 0020h
                jb @@middle_lines

                mov si, offset frame + 4
                call draw_tb_lines
                ret

draw_frame     endp
;--------------------------------------------------------------------

main:
    cli

        mov ax, 3509h                   ; finds out segment and offset of the old 09h handler
        int 21h                         ; now <es> contains segment, <bx> contains offset of Old_int09h_handler

        mov word ptr Old_int09h, bx     ; save old 09h handler
        mov word ptr Old_int09h + 2, es ;

        mov ax, 2509h                   ;
        mov dx, offset New_int09h       ; changing 09h handler into my own
        int 21h                         ;

    sti

if 0
        lea dx, main
        int 27h
endif

        call New_int09h

        Old_int09h dd ?
        
        mov ax, 2509h
        mov dx, word ptr Old_int09h + 2
        mov ds, dx
        mov dx, word ptr cs:Old_int09h;
        int 21h

        mov ax, 4C00h
        int 21h

frame   db  0C9h, 0CDh, 0BBh, 0BAh, 0C8h, 0CDh, 0BCh
;           LT    T     RT    Vert  LB    B     RB

regs            db "ax: bx: cx: dx: si: di: bp: sp: "
reg_vals        dw 8 dup (0)
num_string      db 16 dup (0)
numbers         db "0123456789ABCDEF"

prog_end:

end     Start