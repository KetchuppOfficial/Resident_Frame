.model tiny
.code
.186
org 100h

VIDEOSEG        equ 0B800h
COLOUR          equ 2Bh

Start: jmp main
locals @@


;--------------------------------------------------------------------
;itoa:
;   Input: stack (2 words)
;   Output: AX
;   Registers that change values:
;       AX - number
;       BX - offset of the string
;       CX - is used to put values from one part of RAM to another
;       DX - constains the remainder of division after using mul ()
;       DI - contains radix, than offset of the string
;       SI - constains the remainder of division number by radix
;--------------------------------------------------------------------
itoa        proc

num         equ [bp + 6]
str         equ [bp + 4]

            push bp                 ; prolog
            mov bp, sp              ;

            mov ax, num             ;
            mov bx, str             ; void itoa (int x, char *str, 16)
            mov di, 0010h           ;

            cmp ax, 0000h           ; if (x == 0)
            je @@zero               ;

            cmp ax, 0000h           ; if (x < 0)
            jb @@negative           ;

            jmp @@positive          ; else

@@zero:     mov ch, numbers[0]      ; *str = '0'
            mov ds:[bx], ch         ;
            inc bx                  ; str++

            mov ch, EOL             ; *str = '\0'
            mov ds:[bx], ch         ;

            jmp @@return            ; return

@@negative: 
            mov ch, minus           ; *str = '-'
            mov ds:[bx], ch         ;
            inc bx                  ; str++

            neg ax                  ; x = -x

@@positive:                         
            xor dx, dx              ; do
            div di                  ; x /= 16 и x % 16
            mov si, dx              ; *ptr = "0123456789ABCDEF"[x%16]
            mov ch, numbers[si]     ; (char *ptr = str)
            mov ds:[bx], ch         ;
            inc bx                  ; ptr++

            cmp ax, 0000h           ; while (x != 0)
            jne @@positive          ;
            
            mov ch, EOL             ; *ptr = '\0'
            mov ds:[bx], ch         ;


            mov di, str               
@@change:                           ; do
            dec bx                  ; --ptr
            mov ch, ds:[bx]         ; char c = *ptr

            mov ah, ds:[di]         ; *ptr = *str
            mov ds:[bx], ah         ;

            mov ds:[di], ch         ; *str = c
            inc di                  ; str++

            cmp bx, di              ; while (ptr > str)
            ja @@change             ; 
        
@@return:
            mov ax, str             ; return str

            pop bp                  ; epilog
            ret                     ;

EOL         db '$'
minus       db '-'

numbers     db "0123456789ABCDEF"

itoa        endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;strlen:
;    Input: stack (1 word)
;    Output: AX
;    Registers that change values:
;       AL - contains end of line symbol
;       BX - offset of the beginning of the string
;       DI - offset of the current char
;--------------------------------------------------------------------
strlen      proc

            push bp
            mov bp, sp

            mov al, EOL
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
;New_Line:
;   Registers that change values:
;       DI - offset in the video segment
;--------------------------------------------------------------------
New_Line	proc

            push ax
            push cx
            push dx
            
            xor dx, dx          ;
            mov ax, di          ;
            mov cx, 0A0h        ;
            div cx              ; di = (di/160)*(160 + 1)
            inc ax              ;
            mul cx              ;
            mov di, ax          ;

            pop dx
            pop cx
            pop ax

			ret

New_Line	endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;Draw_TB_Lines:
;   Registers that change values:
;       AL - character (inside <lodsb>)
;       AX - character and its descriptor (inside <stosw>)
;       CX - counter
;       DI - offset in the video segment (inside <stosw>)
;       SI - offset in buffer (inside <lodsb>)
;--------------------------------------------------------------------
Draw_TB_Lines   proc

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

Draw_TB_Lines   endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;Draw_Registers:
;   Registers that change values:
;       AL - charachers
;       AH - background colour
;       AX - video segment (at first), register's value (then),
;            length of the string (after that)
;       BX - offset of the array with registers' values
;       CX - counter
;       DI - offset in the video segment
;       ES - video segment
;       SI - offset of the string
;--------------------------------------------------------------------
Draw_Registers      proc

                    mov reg_vals[0], ax             ;
                    mov reg_vals[2], bx             ;
                    mov reg_vals[4], cx             ;
                    mov reg_vals[6], dx             ; filling array with
                    mov reg_vals[8], si             ; registers' values
                    mov reg_vals[10], di            ;
                    mov reg_vals[12], bp            ;
                    mov reg_vals[14], sp            ;
            
                    mov ax, VIDEOSEG                ;
                    mov es, ax                      ; set video mode
                    mov ah, COLOUR                  ;
                    
                    mov di, 170                     ; top left corner of regs area
                    mov bx, offset reg_vals
                    mov cx, 8                       ; counter for 8 regs
    @@print_regs:
            push cx

                push di
                push bx
                    
                    mov ax, [bx]

                    push ax                         ;
                    push offset num_string          ;
                    call itoa                       ; transform number in a string
                    pop cx                          ;
                    pop cx                          ;

                    mov si, ax

                    push si                         ;
                    call strlen                     ; measure length of the string
                    pop cx                          ;

                pop bx
                pop di

                    cmp ax, 4                       ; if there are 4 digits
                    je @@digits                     ; draw just them
                    
                    mov cx, 0004h                   ; else
                    sub cx, ax                      ; calculate how many zeors
                    mov al, '0'                     ; have to be drawn
                    mov ah, COLOUR      
                @@fill_spaces:                      ;
                    stosw                           ; draw zeors
                    loop @@fill_spaces              ;

                @@digits:
                    mov ah, COLOUR
                @@value:                            ;
                    lodsb                           ;
                    cmp al, EOL                     ; draw other digits    
                    je @@end                        ;
                    stosw                           ;
                    jmp @@value                     ;
                @@end:

                    call New_Line                   ;
                    add di, 10                      ; jump to the next line
                    add bx, 2                       ;

            pop cx
            loop @@print_regs

                    ret

reg_vals    dw 8 dup (0)
num_string  db 17 dup (0)                 

Draw_Registers      endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
;Draw_Frame:
;   Registers that change values:
;       AL - charachers
;       AH - background colour
;       AX - video segment (at first)
;       CX - counter
;       DI - offset in the video segment
;       ES - video segment
;       SI - offset in registers' names array
;--------------------------------------------------------------------
Draw_Frame      proc
            
                mov ax, VIDEOSEG                ;
                mov es, ax                      ; set video mode
                mov ah, COLOUR                  ;
            
                xor di, di                      ; set offset in VIDEOSEG as 0

                mov si, offset frame            ; draw top line
                call Draw_TB_Lines              ; of the frame
            
                mov si, offset frame + 3        ; si = vertical part of the frame 

                xor cx, cx                      ; set counter to 0
            @@middle_lines:
                mov al, [si]                    ; draw left vertical part
                stosw                           ;

        push si

                mov si, offset regs_names       ; si = current regiser's name
                add si, cx                      ;

            push cx

                mov cx, 0004h                   ; 4 chars: for example, "ax: "
            @@before_value:                     ;
                lodsb                           ; prints sth like "ax: "
                stosw                           ;
                loop @@before_value             ;

            pop cx

                add di, 8                       ;
                mov al, 'h'                     ; draws 'h' letter to show that numbers
                stosw                           ; are written in hexadecimal from

        pop si

                mov al, [si]                    ; draw right vertical part
                stosw                           ;
            
                call New_Line                   ; jmp to the new line

                add cx, 4
                cmp cx, 4*8
                jb @@middle_lines

                mov si, offset frame + 4        ; draw bottom line
                call Draw_TB_Lines              ; of the frame
            
                ret

frame   db  0C9h, 0CDh, 0BBh, 0BAh, 0C8h, 0CDh, 0BCh
;           LT    T     RT    Vert  LB    B     RB

regs_names      db "ax: bx: cx: dx: si: di: bp: sp: "

Draw_Frame      endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
main    proc
    
        cli                                     ; extern interrupts are no longer allowed
        
        ; CHANGING INT 08H

            mov ax, 3508h                       ; finds out segment and offset
            int 21h                             ; of the old 08h handler

            mov word ptr Old_int08h, bx         ; save old 08h handler
            mov word ptr Old_int08h + 2, es     ;

            mov ax, 2508h                       ;
            mov dx, offset New_int08h           ; changing 08h handler into my own
            int 21h                             ;

        ; CHANGING INT 09H
        
            mov ax, 3509h                       ; finds out segment and offset
            int 21h                             ; of the old 09h handler

            mov word ptr Old_int09h, bx         ; save old 09h handler
            mov word ptr Old_int09h + 2, es     ;

            mov ax, 2509h                       ;
            mov dx, offset New_int09h           ; changing 09h handler into my own
            int 21h                             ;

        sti                                     ; extern interrupts are allowed again

            mov ax, 3100h                       ;
            mov dx, offset prog_end             ;
            shr dx, 4                           ; makes program resident
            inc dx                              ;
            int 21h                             ;

main        endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
New_int08h  proc
            
            pusha                       ;
            push es                     ; save used registers
            push ds                     ;

            push cs                     ; 
            pop ds                      ;
        
            cmp cs:[Frame_Flag], 01h    ; check <Frame_Flag> value
            jne @@no_frame              ; 

            call Draw_Registers
            call Draw_Frame
@@no_frame:
            mov al, 20h                 ; correct way to return from
            out 20h, al                 ; int 08h handler

            pop ds                      ;
            pop es                      ; restore registers' value
            popa                        ;

            db 0EAh                     ; <jmp far ptr> to old
            Old_int08h dd 0             ; int 08h handler

New_int08h  endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
New_int09h  proc

Open        equ 17h                         ; 'I'
Close       equ 24h                         ; 'J'

            push ax                         ; save used registers

            in al, 60h                      ;
            cmp al, Open                    ; read and check char from the keyboard
            jne @@not_pressed               ;
            
            mov cs:[Frame_Flag], 01h        ; if 'I' button is pressed, Frame_Flag = 1

            in al, 61h                      ;
            mov ah, al                      ;
                                            ;
            or al, 80h                      ;
            out 61h, al                     ; correct way to return from
                                            ; 
            mov al, ah                      ; handler of 09h interrupt
            out 61h, al                     ;
                                            ;
            mov al, 20h                     ;
            out 20h, al                     ;

            pop ax                          ; restore registers' values
            iret                            ;

@@not_pressed:
            pop ax                          ; if 'I' button is not pressed,
                                            ;
            db 0EAh                         ; <jmp far ptr> to old handler
            Old_int09h dd 0                 ;

Frame_Flag  db 0

New_int09h  endp
;--------------------------------------------------------------------

prog_end:

end     Start