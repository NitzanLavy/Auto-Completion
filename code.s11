
tks = 177560
tkb = 177562
tps = 177564
tpb = 177566
lcs = 177546
;emt_mask=177400


.=torg + 1000

main:					mov pc, sp							;Initialize stack
						tst  -(sp)
						mov #input, @#60					;Interrupt vector intialization.
						mov #100, @#62						;Interrupt vector intialization.
						mov #printc, @#64					;Interrupt vector intialization.
						mov #250, @#66						;Interrupt vector intialization.
						mov #clock, @#100					;Interrupt vector intialization.
						mov #300, @#102							;Interrupt vector intialization.
						

						mov #printc,@#20					;iot trap initialization.
						mov #250,@#22
						bis #101, @#tks
						mov #100,@#tps						;Interrrupt and read enable for printer and keyboard
						mov #msg_len,len_ptr				;initialization of pointers
						add #2,len_ptr						;initialization of pointers
						mov #time,timep					;initialization of pointers
						add #4,timep				;initialization of pointers
						mov #time,prn_time			;initialization of pointers
						inc prn_time			;initialization of pointers
						inc buffer_pointer			;initialization of pointers
						inc buffer_add				;initialization of pointers
						
						
						mov #len_msg, -(sp)				;print first messege
						mov #1, -(sp)					;number of messeges to print
						
						jsr pc, printbuf				;print first messege
						
						cmp (sp)+, (sp)+				;restore stack	
						inc intro_flag					;next stage in intro
						
						
waitT:					cmp #1,busy
						beq waitT
						mov #space, -(sp)					;Print arrow and space
						mov #arrow, -(sp)
						mov #2, -(sp)
						jsr pc, printbuf
						jsr pc, busy_print
						cmp (sp)+, (sp)+
						tst (sp)+
						
check_done:				cmp #1,end_prog				;checks if printer isent busy and end of program is reached
						beq finish_main
						jmp Cwatiing

waiting:				cmp #0,busy				
						beq check_done				;if printer isent busy check if end of program is reached
Cwatiing:				mov #Dict, -(sp)			
						jsr pc, revBubble			;when waiting go to revbubble
						tst (sp)+
						jmp waiting
						
finish_main:			halt							;end of program
												
busy_print:				tstb @#tps							;Loop until printer is ready again.
						bpl busy_print
						rts pc
						
busy_key:				tstb @#tks							;Loo until keyboad is ready again.
						bpl busy_key
						rts pc
						
						
						
						
						
	
;beginnig of checkInDict function
;						
;checkInDict function receives address of word from stack and dictionary from common area, and returns if the given word is included in the dictionary.
;
;The register used to branch to the function is pc.
;Input Parameters:	common area - address of dictionary.
;					stack - given word.
;
;Output: 			stack - 0 if given word is not in dict, 1 if it is in dict.
;
;
checkInDict:			mov r2, -(sp)						;Store register in stack.
			            mov r0, -(sp)						;Store register in stack.
						mov r1, -(sp)						;Store register in stack.
						mov 12(sp), r2						;Move given word to r2 register.
						mov #Dict, r1						;Move dictionary to r1 register.
						mov r2, -(sp)						;Store given word in stack.
check_dict_loop:		mov r1, -(sp)						;Store curret word in dictionary in stack.
						tst -(sp)							;Assign free space in stack for output of wordsComapre function.
						jsr pc, wordsCompare				;Call wordsComapre function.
						cmp #0, (sp)+						;If wordsComapre function returned 0, meaning words are equal, jump to end_ok.
						beq end_CID_ok
						tst (sp)+							;Increase stack.
						add #2, r1							;r1 register contains the node of the current dict word, meaning the node that points to the next word.
						cmp #0, (r1)						;If the node to the next string is 0, meaning no more words to compare to.
						beq end_not_CID_ok					;No word in dictionary found equal to given word. Jump to end_nok
						mov (r1), r1						;r1 register contains the beginning of the next string.
						jmp check_dict_loop					;Jump back to check_dict_loop to compare the given word to the next word in dict.

end_not_CID_ok:			mov #0, 12(sp)						;Reached here if none of the words in dictionary found equal to given word.
                        
						jmp finish_checkInDict
							;Move 0 to checkInDict output slot in stack and jump to finish_checkInDict.

end_CID_ok:				mov #1, 14(sp)						;Reached here if one of the words in dictionary found equal to given word.
						tst (sp)+							;Move 1 to checkInDict output slot in stack, increment stack and continue to finish_checkInDict.
finish_checkInDict:		mov r2,-(sp)
						jsr pc,sent_inc
						cmp (sp)+,(sp)+
						mov (sp)+, r1						;Return register's original value to register.
						mov (sp)+, r0;						Return register's original value to register.
						mov (sp)+, r2						;Return register's original value to register.
						rts pc
						

		


;end of checkInDict function
					
						
						

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;init buffer
;initiats buffer, function puts '@' in every char in buffer


init_buf:				mov r0,-(sp)			;initiates buffer to inital value
						mov r1,-(sp)			;store registers in stack
						mov #12,r0				;number of chars in buffer
        				mov #buffer,r1			;moves buffer to r1
initBf_lp:				movb #'@,(r1)			;moves all chars in buffer to @
						incb r1					;r1 points to next char
						sob r0,initBf_lp		;subtract 1 for r0, number of chars in buffer
						mov #buffer,buffer_pointer ;restores buffer pointer to initial place
						inc buffer_pointer			
						mov (sp)+,r1				;restore registers
						mov (sp)+,r0				;restore registers
						rts pc
						
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;keyboard input						
;intro inputt subsequnces:
;subEnd: ends of the subsquences
;sub1: sores the number of words from buffer to len
;sub3: store time from buffer to time

						;end of the input intro methods
sub3End:				 mov #100, @#lcs		;end of the initiate time method	
Iend_sub:                inc intro_flag			;advance to next stage in intro
						 jsr pc,init_buf		;initiates buffer	
						 mov #endl,-(sp)		;prints end line
						 mov #1,-(sp)			;prints end line
						 jsr pc,printbuf		;prints end line
						 cmp (sp)+,(sp)+		;restore registers
						 jmp back_from_handle	
						 
sub1:                   dec buffer_pointer				;stores the number of words from buffer
sub1Lp:					cmpb #'@,@buffer_pointer		;checks end of word
						beq Iend_sub					;move to finnish sequence
						movb @buffer_pointer,@len_ptr	;moves buffer char to len
						dec buffer_pointer				;next char
						dec len_ptr						; next place in len
						jmp sub1Lp

sub3:                   dec buffer_pointer				; stores time of program from buffer
sub3Lp:					cmpb #'@,@buffer_pointer		;checks end of word	
						beq sub3End						;move to finnish sequence
						movb @buffer_pointer,@timep		;moves buffer char to time
						dec buffer_pointer				;next char
						dec timep						; next place in time
						jmp sub3Lp						

clear_buf:				mov #0,buf_clr					;reached when signaled to initate buffer and moves to init buffer
						jsr pc,init_buf					;moves to clear buffer
						jmp input				
dec_stack:	tst (sp)+									;decrease stack
			jmp back_add_to_dict
							
;reached when user pressed space to add a word to messege							
handle_space:			mov #0, prt_buf
						mov #0,retry				;initiate flags
						mov #0,prt_buf
						mov #0,ready_input			;initiate flags
						mov #endl,-(sp)				;prints end line
						mov #1,-(sp)				;prints end line
						jsr pc,printbuf				;prints end line
						cmp (sp)+,(sp)+				;prints end line
						jsr pc,busy_print			;prints end line
						mov #buffer_add, -(sp)		;checks if word is allready in dict
						tst -(sp)					;place in stack for output
						jsr pc, checkInDict			;checks if word is allready in dict
						cmp #1, (sp)+				;checks answer for check in dict
						beq dec_stack				;restore stack
						tst (sp)+					;restore stack
						jsr pc,add_to_dict			;if not in dict add to dict	
back_add_to_dict:		mov buffer_add,-(sp)		;store buffer in stack
						jsr pc,count_break			;checks if reached last word
						tst (sp)+					;restore stake
						cmp #1,end_prog				;checks if reached end
						beq temp_back_from_handle		;if yes go to finish
						mov #buffer, copy_buffer		;reached when user pressed space to include word in messege
						inc copy_buffer					;creats copy of buffer pointer
						mov #endl, -(sp)				; stores the messeges in stack
						mov #space, -(sp)				;Print arrow and space
						mov #arrow, -(sp)				;Print arrow and space
						mov #endl2,-(sp)				;prints end line
						mov #endl1,-(sp)				;prints end line
						mov #endl2,-(sp)				;prints end line
						mov #endl1,-(sp)				;prints end line
						mov #time2,-(sp)				; stores the messeges in stack
						mov prn_time,-(sp)				; stores the messeges in stack
						mov #time1,-(sp)				; stores the messeges in stack
						mov #endl2,-(sp)		
						mov #endl1,-(sp)				; stores the messeges in stack
						mov #print2_sent,-(sp)			; stores the messeges in stack
						mov #sentence,-(sp)				; stores the messeges in stack
						mov #print1_sent,-(sp)			; stores the messeges in stack
						mov #endl2,-(sp)				;endline
						mov #endl1,-(sp)				;endline
						mov #add2,-(sp)					; stores the messeges in stack
						mov copy_buffer,-(sp)					; stores the messeges in stack
						mov #add1,-(sp)					; stores the messeges in stack
						mov #24,-(sp)					; number of messeges to print
						jsr pc,printbuf
						add #52,sp 					 ;restore stack
													;if word is not in dict word is added to dict
		
						mov #1,buf_clr				;signals to clr buffer
						jmp back_from_handle	
						
t_handle_space:			jmp handle_space			
temp_back_from_handle:	jmp back_from_handle


;;reached if user enterd enter in intro
handle_enter:			cmp #5,intro_flag			
						blt back_from_handle		;if reached after intro go back 
						cmp #1,intro_flag			;if reached first stage of intro-input length
						beq temp1					
						cmp #3, intro_flag			;if reached third stage , input time
						beq temp3
						jmp back_from_handle					
tclr_buf:			jmp clear_buf
temp1:              jmp sub1						
temp3:              jmp sub3						
blink_handle:			jsr pc, end_blink			;reached to end blink	

;;;;;;;;;;;;;;;input,;;;;;;;;
;;reached when user entered input
;checks input and handles it accordingly	
input:					cmp #1,blink_flag				
						beq blink_handle
						cmp #1,buf_clr				;checks if need to clr buffer
						beq tclr_buf
						mov r0, -(sp)				;store registers in stack
						mov r1, -(sp)				;store registers in stack
						mov #0, blink_flag
						jsr pc, busy_key
						movb @#tkb, (r0)			;gets input from user
						bic #177600, (r0)
						cmp #1,flag_nw				;checks if user enterd answer to word suggestion
						beq handle_ans
						cmp #15, (r0)				
						beq handle_enter			;if user enterd enter in intro
						cmp #56, (r0)
						beq handle_dot							
						cmp #10, (r0)
						beq handle_backspace		;if user enterd backspace to erase words from buffer
						cmp #40, (r0)
						beq t_handle_space      	;if user enterd space to incude word in messege
						cmp #41, (r0)
						beq handle_mark				; if user enterd ! to get suggestions from program
						movb (r0), @buffer_pointer
						inc buffer_pointer
						movb (r0), input_buffer			;increases buffer pointer and stores char in buffer
						mov #input_buffer, -(sp)
						jsr pc, print_input				;goes to print buffer
						jsr pc, busy_print
						tst (sp)+
back_from_handle:		bis #1, @#tks					;ready input
						mov (sp)+, r1					;restore registers
						mov (sp)+, r0
						rti


						handle_dot:				mov #finish2,-(sp)	;stores messeges in stack
						mov #endl2,-(sp)	
						mov #endl1,-(sp)
						mov #print2_sent,-(sp)
						mov #sentence,-(sp)
						mov #print1_sent,-(sp)
						mov #endl2,-(sp)
						mov #endl1,-(sp)
						mov #finish1,-(sp)
						mov #endl2,-(sp)
						mov #endl1,-(sp)
						mov #endl2,-(sp)
						mov #endl1,-(sp)
						mov #15,-(sp);number of messseges to print
						jsr pc,printbuf
						add #34,sp
						mov #1,end_prog
						jmp back_from_handle
;reached if user entered answer for suggestion
handle_ans:             cmp #0,err				;checks errors to see if perfect prefix
						bne no_pre				;if not prefect prefix gos to diffrent handle
						cmp #40, (r0)			;if user wants the word in messege
						beq yes
                        cmp #15, (r0)
						beq no					;if user wants the word in messege
						jsr pc, prt_time		;if user enterd an invalid answer
						jmp back_from_handle
						
;reached if user wantes to erase data that he wrote						
handle_backspace:		mov #backspc,-(sp)					;stores backspace and space in stack to print
						mov #space,-(sp)
						mov #backspc,-(sp)
						mov #3,-(sp)
						mov #1,retry						;signals printc to stay in same line
						jsr pc,printbuf						;goes to printbuf to print bakspace
						mov #0,retry
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+						;restore stack
						movb #'@,@buffer_pointer			;erases char in pointer
						dec buffer_pointer
						jmp back_from_handle	

handle_mark:			;reached if user wants suggestions for words
						movb #'@, @buffer_pointer
						jsr pc, init_prefix					;gos to init prefix to initiate prefix loop for new word
						jmp back_from_handle						
						
;if user wants word in messege, prints messeges accordingly and adds word to messege					
yes:					mov @CR_dict,word_cp		
						mov CR_dict,-(sp)				;store word to add to messege
						jsr pc,sent_inc					;stores word in messege
						tst (sp)+
						mov word_cp,-(sp)
						jsr pc,count_break				;increses the number of words in messege
						tst (sp)+
						cmp #1,end_prog					;checks if end of prog flag is on
						beq back_from_handle
						mov #0,flag_nw					;raises flags
						mov #1,ready_input				;raises flags
						mov #time2,-(sp)    			   ;stores messeges in stack
						mov prn_time,-(sp)				;stores messeges in stack
						mov #time1,-(sp)				;stores messeges in stack
						mov #endl2,-(sp)                 ;stores messeges in stack
						mov	#endl1,-(sp)                ;stores messeges in stack
						mov #print2_sent,-(sp)          ;stores messeges in stack
						mov #sentence,-(sp)				;stores messeges in stack
						mov #print1_sent,-(sp)			;stores messeges in stack
						mov #endl2,-(sp)			    ;stores messeges in stack
						mov	#endl1,-(sp)				;stores messeges in stack
						mov #add2,-(sp)					;stores messeges in stack
						mov word_cp,-(sp)				;stores messeges in stack
						mov #add1,-(sp)					;stores messeges in stack
						mov #15,-(sp)					;number of messseges to print
						jsr pc,printbuf	
						cmp (sp)+,(sp)+  				 ;restore stack
						cmp (sp)+,(sp)+					;;restore stack
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						jsr pc,init_buf						;iniitates buffer
						jmp back_from_handle
												
		

;reached if user doesent want our suggestion for the word, goes back to prefix loop with same data						
no:						jsr pc, prefix_loop		;goes to prefix loop with same data
						jmp back_from_handle
						
						
						
;reached if the answer the user is giving is not a perfect prefix for the input word						
no_pre:					cmpb #'y,(r0)   			 ;uesr pressed y for yes
						beq yes
						cmpb #'n,(r0)					;uesr pressed n for no
						beq no
						jsr pc, prt_time  				;reached if anwer is invalid
						jmp back_from_handle
						
						
						
	;reached if answer is invalid will print time and buffer to try again					
prt_time:               mov #2,retry  						
						mov #0,flag_nw						;signals the end of check to currernt word
						mov #time2,-(sp)					;stores messeges in stack
						mov prn_time,-(sp)					;moves time pointer to stack
						mov #time1,-(sp)
						mov #3,-(sp)
						mov #1,prt_buf						;signals to print buffer when finish
						jsr pc,printbuf						;goes to print
						cmp (sp)+,(sp)+						;restore registers
						cmp (sp)+,(sp)+
						rts pc

						

						

						




;adds a word to dict
;adds word from buffer

add_to_dict:			mov r1,-(sp)  					;store registers
						mov #Dict, r1					;move dict to r0
add_dict_loop:			cmpb #0, @(r1)					;check if end of word
						beq before_word_to_dict		
						add #2, r1						;next dict word
						mov (r1), r1    				 ;next dict word
						jmp add_dict_loop
before_word_to_dict:	mov #buffer, buffer_pointer
						inc buffer_pointer
						mov (r1), -(sp)
word_to_dict:			cmpb #'@, @buffer_pointer			;copys word to dict
						beq end_add_to_dict
						movb @buffer_pointer, @(r1)
						inc (r1)
						inc buffer_pointer
						jmp word_to_dict
end_add_to_dict:		movb @buffer_pointer, @(r1)
						mov (sp)+, (r1)
						mov (sp)+,r1
						rts pc



;count the number of words in messege if reached len then move to end sequence
;reached every time a word was added to messege						
count_break: 			mov #msg_len,len_ptr			;counts the number of words the user enterd the messege
						add #2,len_ptr
						cmpb #'@,@len_ptr
						beq end_sequence
						cmpb #'0,@len_ptr				;checks if current cha
						beq  fin_check        			 ;checks if finnish prog
						decb @len_ptr		
						cmpb #'0,@len_ptr
						beq fin_check
						rts pc
;decreases len by 10						
fin_check:				mov #'7,@len_ptr
						dec len_ptr
						cmpb #'0,@len_ptr
						beq end_sequence  				;if finnish prog got to end sequence
						decb @len_ptr
						inc len_ptr
						rts pc


						

						
						
;end sequnce prints messges reached when all words were added to messege
end_sequence:			jsr pc,busy_print
						mov	r0,-(sp)					;store the last word addedin r0
						mov 4(sp),r0		
						mov #0,ready_input					;initiates falgs
						mov #0,flag_nw
						mov #0,retry
						mov #finish2,-(sp)					;stores messeges in stack
						mov #endl2,-(sp)					;stores messeges in stack
						mov #endl1,-(sp)					;stores messeges in stack
						mov #print2_sent,-(sp)			;stores messeges in stack
						mov #sentence,-(sp)				;stores messeges in stack
						mov #print1_sent,-(sp)			;stores messeges in stack
						mov #endl2,-(sp)				;stores messeges in stack
						mov #endl1,-(sp)				;stores messeges in stack
						mov #finish1,-(sp)				;stores messeges in stack
						mov #endl2,-(sp)				;stores messeges in stack
						mov #endl1,-(sp)					;stores messeges in stack
						mov #endl2,-(sp)					;stores messeges in stack
						mov #endl1,-(sp)				;stores messeges in stack
						mov #print2_sent,-(sp)				;stores messeges in stack
						mov #sentence,-(sp)					;stores messeges in stack
						mov #print1_sent,-(sp)				;stores messeges in stack
						mov #endl2,-(sp)				;stores messeges in stack
						mov	#endl1,-(sp)					;stores messeges in stack
						mov #add2,-(sp)					;stores messeges in stack
						mov r0,-(sp)					;stores messeges in stack
						mov #add1,-(sp)					;stores messeges in stack
						mov #25,-(sp)					;number of messges to print
						jsr pc,printbuf					;prints messeges
						add #56,sp						;restore rejisters
						mov #1,end_prog				;rais end prog flag
						rts pc
;;;;;;;;;;;;;;;;reached to end blink
;waits for blink to end and signals to stop		
end_blink:		
						cmp #2,space_flag				;if ends blink only when space flag=2
						bne end_blink					;waits until blink flag is of
						clr busy						;clrs output flag
						mov #0,blink_flag				;initates flags
						mov #0,retry
						jsr pc,busy_print		
						mov #endl,-(sp)					;prints end line
						mov #1,-(sp)		
						jsr pc,printbuf					;prints end line
						cmp (sp)+,(sp)+
b_wait:					cmp #1,busy		
						beq b_wait
						rts pc
	

	
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;increase scentence				
				
sent_inc:           mov r0,-(sp)						;adds a new word to sentence
					mov 4(sp),r0						;stores word to add in r0
					mov r1,-(sp)	
					mov (r0),r1							;store begining of word in r1
					cmp sentSt,sentp						;checks place of pointer
					beq copy_char
					movb #40,@sentp							;replace @ with space
					inc sentp
copy_char:          movb @(r0),@sentp						;loop copys word until end of word
					cmpb #'@,@(r0)							;checks end of word
					beq inc_end
					inc (r0)								;next char in word
					inc sentp
					jmp copy_char
inc_end:            mov r1,(r0)								;restors pointers and registers
					mov (sp)+,r1
					mov (sp)+,r0
					rts pc			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;clock 
;counts time and restors time pointers
;if end of time is reached prints out of time messeges		
						
clock:					mov #time,prn_time					;moves pointers to place
						inc prn_time						;restore time  pointers
						mov #time,timep						;restore time  pointers
						add #4,timep						;restore time  pointers
						inc time_counter					;Increase time_counter each clock interrupts.
						cmp time_counter, rate				;If one second has passed:
						bne clk_finish	
						
						mov #0, time_counter				;Reinitialize the time_counter again to start a new second.
						cmpb #'0, @timep						;If time is zero:
						beq dect								;goes to decrease time
					
						decb @timep																;Decrease time by 1 second.
clk_finish:				cmpb #'0,@prn_time					;moves the print time pointer to place
						beq prn_nxt							;puts the print time pointer in place
						rti	
prn_nxt:				inc prn_time						;moves the print time pointer to place	
						jmp clk_finish
												
						
dect:                   movb #'7,@timep						;decrease time if 0 replace with 7 and move to decrease next culom in time
						dec timep
						cmpb #'0, @timep					;checks if 0
						beq dect
						cmpb #'@,@timep						; if reached @ - end of time
						beq clk_timeOut						;go to print timeOut sequence
						decb @timep
						mov #time,timep						;restore time pointer
						add #4,timep
						
						jmp clk_finish
						
;;;end of time is reached , prints out of time messeges						
clk_timeOut:			mov #0,ready_input						;reached if time ran out prints messeges and raisess flag
						mov #finish2,-(sp)						;store messeges in stack
						mov #endl2,-(sp)						;store messeges in stack
						mov #endl1,-(sp)					;store messeges in stack
						mov #print2_sent,-(sp)					;store messeges in stack
						mov #sentence,-(sp)					;store messeges in stack
						mov #print1_sent,-(sp)					;store messeges in stack
						mov #endl2,-(sp)						;store messeges in stack
						mov #endl1,-(sp)						;store messeges in stack
						mov #timeOut,-(sp)						;store messeges in stack
						mov #endl2,-(sp)						;store messeges in stack
						mov #endl1,-(sp)						;store messeges in stack	
						mov #13,-(sp)							;number of words to print
						jsr pc,printbuf							;;prints messeges
						cmp (sp)+,(sp)+							;restore stack
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						mov #1,end_prog								;raises end programe flag
						clr @#lcs
						rti



;;;;;;;;;;;;;;;;;;;;;;;;;; end clock

;;;;;;;;;;;;;;;;;;;;;;;;;; print
;;;;handles arrow char signals printc not to end line
arrow_char:				mov #2, end_line_flag							;if arrow char changes flag to stop end line
						jmp cont_print


;;;;prints space as part of blink sequence
pr_space:				mov #40,@#tpb								;prints space
						mov #0,space_flag							;next spage in blink
						rti

;;;;;prints input
;reseves char to print forom stack
print_input:			tst busy							;a buffer for printing chars from input one by one without ending line
						bgt print_input
						mov #1, is_input					;signals printc not to end line
						mov 2(sp), @words					;char to print to words
						mov #1, count						;number of words to print
						mov #0, blink_flag					;turn off blink flag
						mov #1, busy						;turn on busy
						mov @words, pointer					;moves pointer to first char
						iot		;print
						rts pc


						
						
;;;;;;;;;;;;;;;;;print buffer 
;reseves number of meseges to print from stack
;reseves the addresses of messeges from stack
;copys the addreses to words and points pointer to first word									
printbuf:				tst busy						;checks printer isent busy
						bgt printbuf	
						mov #print1,words				;moves the first word to words
						mov #0, is_input				;signals not input
						mov words,wordStart				;initate wordStart pointer
						mov 2(sp),count					;retreves number of words from stack
						mov count,countcp				;copys count
						cmp (sp)+,(sp)+
initWords:				mov (sp)+,@words				;puts word from stack to words
						add  #2,words					;move to next word
						dec countcp						;counter for number of words
						cmp  #0,countcp
						bne initWords					;if count not done goes to initwords to put next word in place
						mov count,countcp		
						add #2,countcp					;restore count
						sub  countcp,sp					;restors stack to place
						sub  countcp,sp
						mov #1, busy					;turn busy on
						mov  wordStart,words			;puts pointer in place
						mov  @words,pointer
						iot
						rts pc
;;;;;reached from iot
; prints messeges from words via pointer
;counts the number of messeges via count		
printc:					cmp  #1,space_flag				;checks space flag for stage at blink
						beq  pr_space
						cmpb #'@,@pointer
						beq next_word					;checks end of word
						cmpb #76 ,@pointer   			 ;checks if arrow to signal no to end line
						beq arrow_char
						cmp #1,retry					;checks flag for no end line
						beq arrow_char			
cont_print:				movb @pointer,@#tpb				;prints char
						inc pointer						;moves pointer to next word
						
						rti
next_word:  			cmp #1,count					;reached to print next word moves pointer to next word								
						beq end 						;check if done printing
						dec count						;decrease number of words var
						inc pointer   
						
						add #2,words					;next word
						mov @words,pointer				; move to next word
						
						jmp printc
end:       				cmp #1, is_input				;reached at end of output checks flags to continue
						beq zero_print_flag				;check is input flag
						cmp  #1,blink_flag				;if blink flag move to blink
						beq  blink		
						cmp #0, end_line_flag			;end of line stage
						bne printf_end_line
						movb #12,@#tpb					;end of line stage 1
						inc end_line_flag
						jmp end_printing				
printf_end_line:		cmp #1, end_line_flag		
						bne zero_print_flag
						movb #15,@#tpb       			 ;end of line stage 2
						inc end_line_flag				;increase end of line flag
						jmp end_printing
zero_print_flag:		clr end_line_flag			
						cmp #2, intro_flag				;check to print intro messges
						beq sub2
						cmp #4, intro_flag				;check to print intro messges 4
						beq sub4
						cmp #5, intro_flag				;check to print intro messges 5
						beq sub5
						cmp #1,ready_input				; checks to ready input
						beq rdy_inpt
						cmp #0,prt_buf					;checks to print buffer
						bne print_buffer
						clr busy
						mov pointerStart, pointer		;moves pointer to start
end_printing:			rti


;prints the current word in buffer
print_buffer:			cmp #1,prt_buf					;checks buffer flag
						beq buf1			
						mov #0,prt_buf	
						mov buffer_add,-(sp)			;prints buffer
						mov #1,-(sp)					;number of words
						dec retry
						clr busy
						jsr pc,printbuf
						cmp (sp)+,(sp)+
						rti
						
						
buf1:					clr busy						;prints space befor buffer
						movb #15,@#tpb					;prints space
						mov #1,ready_input
						mov #2,prt_buf
						rti
						
						
						
						
						
						

rdy_inpt:				clr busy						;readys input 
						mov #0,ready_input	
						mov #space, -(sp)				;Print arrow and space
						mov #arrow, -(sp)
						mov #2, -(sp)					;num of words to print
						jsr pc, printbuf				;move to print
						cmp (sp)+,(sp)+					;restore stack
						tst(sp)+
						rti




;;;;;;;;;blink sequence
;;;;has 4 stages:
;prints backspace
;prints space
;prints backspace 
;prints last char						
blink:                  movb #8,@#tpb    				;blink sequence prints backspace 
						cmp #0,space_flag				;if space flag =0move to decrease pointer
						beq nxtp
						mov #1,space_flag				;if space flag = 2 move space flag to 1 to print space next char
						rti
						
nxtp:					dec pointer						;decrease pointer in blink
						mov #2,space_flag				;next stage in blink
						rti
						






					
;;;;;;;;;;;;;;;;;end print

;;;;;;;;;;;;;;;;print messages in the beginning (need to move up when finished writing)
								
						
								
;;;;;;prints intro messeges
;sub2 prints enter time messege
;sub4 prints "you may enter ...." messege
;sub5 prints "dont worry...." messege
						
sub2:					clr busy						;prints second intro messege
						mov #time_msg, -(sp)			;prints time messege
						mov #1, -(sp)
						mov #1,ready_input				;readesy for input
						jsr pc, printbuf				;move to print messege
						cmp (sp)+, (sp)+
						jmp end_sub

						
sub4:					clr busy						;prints third intro messege
						mov #start1_msg, -(sp) 
						mov #1, -(sp)
						jsr pc, printbuf				;move to  print messege
						cmp (sp)+, (sp)+
						jmp end_sub

sub5:					clr busy						;prints fourth intro messege
						mov #start2_msg, -(sp)
						mov #1, -(sp)			
						mov #1,ready_input				;readys for input at end of print
						jsr pc, printbuf				;move to  print messege
						cmp (sp)+, (sp)+
						jmp end_sub
						
 

end_sub:                inc intro_flag					;back from intro msg
						rti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;beginning of isPrefix loop
	


;;init_prefix
;reached when starting to check a new word	
;initates dict
;initates histogram
init_prefix:  	 		mov #0,retry					;initiates prefix loop for new word
						mov #endl,-(sp)
                        mov #1,-(sp)					; prints end line
						jsr pc,printbuf
						cmp (sp)+,(sp)+
						mov #Dict,CR_dict
						mov #0, err							;iniate the cuurent error variable	
						mov #0,flag_nw						;flag for new word
						jsr pc,init_count					;method that iniates the hisogram array for words sent 
													
						jsr pc,prefix_loop					;go to prefix loop
						rts pc
	
init_count: 			mov r5, -(sp)					;initates histogram array for words sent
						mov r1, -(sp)
						mov CR_dict,r1					;moves dict to r1
						mov #words_hist, pointer_hist
						mov #50, r5						;max number of dict words
init_loop:  			cmp #0,r1						;checks if end of dict
						beq end_init
						cmpb #0,@(r1)	
						beq empty						;if reached empty word gos to empty to signal words of limits
						movb #0, @pointer_hist			;moves 0 for every word in dict
						inc pointer_hist				;next word in hist
						add #2,r1              			;;next dict
						mov (r1),r1						;next dict
						sob r5, init_loop
end_init:				mov (sp)+, r1					;restore registers
						mov (sp)+, r5
						mov #words_hist, pointer_hist
						rts pc

empty:					cmp #0,r1						;if empty word moves 1 so prefix loop will know to skip
						beq end_init					;checks end
						movb #1,@pointer_hist			;move 1 to signal word off limits
						inc pointer_hist
						add #2,r1
						mov (r1),r1						;next dict word
						sob r5,empty

prefix_loop:			mov r0, -(sp)						;r0-current dict
						mov r1, -(sp)						;r1 current word
						mov r2, -(sp)						;r2 current err
						mov r3, -(sp)						;r3 current dict addres			
						mov r5, -(sp)						;max er
						mov X, r5							;sores the max error allowed
						mov err, r2							;error to isprefix
						mov CR_dict, r3						;store the current dict word
						mov (r3), r0						;dict word to is prefix
						cmp #1,flag_nw						;checks  if new word 
						beq next_dict						;if new word if not moves the dict word via next dict
prefix_check:   		mov #buffer_add, r1					;moves the current word to check to r1 for isPrefix		??????????/
						jsr pc, isPrefix
						mov r0,(r3)									;check if prefix
						cmp #1, r1							;checks the answer of isPrefix
						beq word_ok
						jmp	next_dict						;if prefix go to word ok to ask user if word is ok;


						
next_dict :   			add #2, r3							;reached to find next dict word
						mov (r3),r3
nxtd:					inc pointer_hist
						cmp #0, r3			
						beq next_err					;moves the iterator for the word_count array to the next place
						mov (r3), r0
						cmpb #0,(r0)
						beq next_err						;moves the next dict word to r0						;checks if end of dict
						cmpb #1, @pointer_hist 				;checks if current dict word was allready checked
						beq next_dict						;if allready checked skip to next dict
						jmp prefix_check					;checks next dict word in prefix loop
				
next_err:       		inc r2								;reached if finished all dict words, increases error count and starts over
						mov #words_hist, pointer_hist 		;moves iterator to beginng of array
						cmp r2, r5							;checks if reached error limit
						bgt end_nok							;if yes jumps to end
						mov #Dict,r3						;moves dict to first word
						mov (r3),r0
						mov #words_hist, pointer_hist
						cmpb #1, @pointer_hist 				;checks if current dict word was allready checked
						beq next_dict
						jmp prefix_check

word_ok:       			cmp #1,busy							;reached if isprefix returned 1
						beq word_ok
						mov #1,blink_flag					;turns blink on
						mov #1,retry						;turns end line
						cmp #0,r2							;checks if perfect prefix
						beq pref_fix						;if yes goes to print different messege
						mov #fix2_msg,-(sp)					;reached if found a prefix word
						mov r0,-(sp)					
						mov #fix1_msg,-(sp)
						mov #3,-(sp)						;number of messeges to print
						jsr pc,printbuf						;sends messeges to output to ask user if word is ok
						cmp (sp)+,(sp)+
						cmp (sp)+, (sp)+
save:					mov #1,flag_nw						;raises flag for word sent
						mov r3,CR_dict						;saves current data in global variabels to use in further calles
						movb #1,@pointer_hist				;increases the histogram place for current dict word from 0 to 1 ti signal word sent
						
						mov r2,err							;store current error check
						jmp finish							;moves to finish to wait for input
				
pref_fix:      			
						mov r0,-(sp)						;if word found is a perfect prefix
						mov #space,-(sp)					;prints arrow, space and word found
						mov #arrow, -(sp)
						mov #3,-(sp)						;numbere of words to print
						jsr pc,printbuf						;go to print buf
						cmp (sp)+,(sp)+						;restore regisers
						cmp (sp)+,(sp)+
						jmp save							;jmp to save current content
		

;reached if no prefix was found
end_nok:       			mov #2,retry
						mov #time2,-(sp)					;stores time messege in stack
						mov prn_time,-(sp)
						mov #time1,-(sp)
						mov #endl2,-(sp)
						mov #endl1,-(sp)
						mov #1,prt_buf
						cmp #0,flag_nw  						;reached if no dict word found that the user wantes
						beq no_words						;checks flag foor words sent if no word was sent back to user moves to no_words to send output
						mov #no2_match, -(sp)				;sores messgeg in stack
						mov buffer_add, -(sp)
						mov #no1_match, -(sp)
						mov #10,-(sp)						;nember of words to print
						mov #0,flag_nw						; initiates flagnw
						jsr pc,printbuf						;sends output messege for no more words left in dict
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						tst (sp)+
						jmp finish							;moves to finish to wait for input
			   
			   ;reached if no words at all were found
no_words:      			mov #no2_fix,-(sp)					;reached if no prefix words were found 
						mov buffer_add, -(sp)				;store messeges in stack
						mov #no1_fix, -(sp)
						mov #10, -(sp)						;number of messeges to print
						jsr pc,printbuf						;sends messege to user
						cmp (sp)+,(sp)+						;restore stack
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						cmp (sp)+,(sp)+
						tst (sp)+
						mov #0,flag_nw						;initiates flag nw
						jmp finish							;move to finish to wait for input
			   
finish:		   												;reached at end of method recovers registers and returns to main
						mov (sp)+, r5
						mov (sp)+, r3						;restore stack
						mov (sp)+, r2
						mov (sp)+, r1
						mov (sp)+, r0
						rts pc
				
;beginning of isPrefix function
;						
;isPrefix function receives a string and a word, and checks if the given input string is a prefix of the given word.
;The register used to branch to the function is pc.
;
;Input Parameters:	r0 - input word
;					r1 - input string to check
;					r2 - number of errors allowed
;
;Output: 			r1 - 1 is its a prefix, 0 if its not
;
;		
isPrefix:				mov r0, -(sp)						;Store register in stack.
						mov r3, -(sp)						;Store register in stack.
						mov r5, -(sp)						;Store register in stack.
						mov (r1), r5						;Move input string's first char's address into r5 register.
						mov #0, r3							;Init error register.
						mov #2, r1							;Init indicator register for success of prefix.
						
						jsr pc, IPLoop						;Jump to IPLoop label to start checking recoursivly if the string is a prefix of word.
						
IP_finish:				mov (sp)+, r5						;Return register's original value to register.
						mov (sp)+, r3						;Return register's original value to register.
						mov (sp)+, r0						;Return register's original value to register.
						cmp #1, r1							;Check if r1 contains 1, meaning the input string is a prefix of the word.
						beq IPEnd
						mov #0, r1							;If r1 doesnt contain 1 it means the input string is not prefix.
IPEnd:					rts  pc							
						
;Recoursive loop of isPrefix to check if the input string is a prefix of a given word.					
IPLoop:					cmpb #'@, (r5)						;Check if string input reached the end.
						beq EndLoop							;If it is, jump to EndLoop.
						
						cmpb (r5), (r0)						;Check if current char of input string and word are equals.
						beq IPNextChar						;If they are, jump to IPNextChar.
						
						cmpb #'@, (r0)						;Check if word reached the end.
						beq EndOfWord						;If it is, jump to EndOfWord.
						
						inc r3								;If none of the conditions above were true, it means none of the strings reached the end nor
															;their current chars are equal. Increasing the number of errors.
						cmp r3, r2							;Check if the number of errors are above the allowed amount.
						bgt IPLoopEnd						;If it is, jump to IPLoopEnd.
						
						inc r5								;Move to the next char in input string.
						mov r3, -(sp)						;Save current amount of errors in stack.
						mov r0, -(sp)						;Save current char of word in stack.
						mov r5, -(sp)						;Save current char of input string in stack.
						jsr pc, IPLoop						;Jump to IPLoop to continue the comparison.
						mov (sp)+, r5						;Return current char of input string from stack.
						mov (sp)+, r0						;Return current char of word from stack.
						mov (sp)+, r3						;Return current amount of errors from stack.
						cmp #1, r1							;If the value of r1 after the return from the jump is 1, it means that there is at least
															;one way in which input string is a prefix of word. Jump to IPLoopEnd to continue to return 1.
						beq IPLoopEnd
						
						inc r0								;Move also to the next char in word.
						mov r3, -(sp)						;Save current amount of errors in stack.
						mov r0, -(sp)						;Save current char of word in stack.
						mov r5, -(sp)						;Save current char of input string in stack.
						jsr pc, IPLoop						;Jump to IPLoop to continue the comparison.
						mov (sp)+, r5						;Return current char of input string from stack.
						mov (sp)+, r0						;Return current char of word from stack.
						mov (sp)+, r3						;Return current amount of errors from stack.
						cmp #1, r1							;If the value of r1 after the return from the jump is 1, it means that there is at least
															;one way in which input string is a prefix of word. Jump to IPLoopEnd to continue to return 1.
						beq IPLoopEnd
						
						dec r5								;Move to the previous char of input string.
						mov r3, -(sp)						;Save current amount of errors in stack.
						mov r0, -(sp)						;Save current char of word in stack.
						mov r5, -(sp)						;Save current char of input string in stack.
						jsr pc, IPLoop						;Jump to IPLoop to continue the comparison.
						mov (sp)+, r5						;Return current char of input string from stack.
						mov (sp)+, r0						;Return current char of word from stack.
						mov (sp)+, r3						;Return current amount of errors from stack.

						jmp IPLoopEnd						;All of the checks are finished, jump to IPLoopEnd to return the outcome.
												
EndLoop:				cmp r3, r2							;Reached here if the end of input string is reached. If the current amount of errors
						ble IPSuccessFinish					;is legal jump to IPSuccessFinish, else jump to IPLoopEnd.
						jmp IPLoopEnd
						
IPNextChar:				inc r0								;Reached here if the current chars of input string and word are equal.
						inc r5								;Move to the next char of both strings, and jump back to IPLoop
						jmp IPLoop							;to continue the comparison.
						
EndOfWord:				inc r3								;Reached here if the end of word is reached. Since input string didn't reach the end,
						inc r5								;increase the amount of errors by 1. Also, move to the next char in input string to continue
						jmp IPLoop							;the comparison, and jump back to IPLoop.
			
IPSuccessFinish:		mov #1, r1							;Reached here if the input string is a prefix of word. move 1 into r1, and finish the recoursive loop.
						
IPLoopEnd:				rts pc

;end of isPrefix function



;beginnig of revBubble function
;						
;revBubble function receives address of dictionary from stack, and sort the dictionary using bubble sort algorythm.
;
;The register used to branch to the function is pc.
;Input Parameters:	stack - address of dictionary.
;
;Output: 			no output.
;
;

revBubble:				mov r5, -(sp)						;Store register in stack.
						mov r2, -(sp)
						mov r3, -(sp)
									;Store register in stack.
						mov 10(sp), r2						;Move Dict to r2 register.
						jsr pc, recRevBubble				;Start Recursive Bubble Sort.
						mov (sp)+,r3
						mov (sp)+, r2						;Return register's original value to register.
						mov (sp)+, r5						;Return register's original value to register.
						rts pc
					
recRevBubble:			tst (r2)+
						cmp #0, (r2)						;If end of dict reached jump to finish.
						beq finish_revBubbleRec
						mov (r2),r3
						cmpb #0,@(r3)
						beq finish_revBubbleRec						;Move r2 register to point on the node.
						mov -(r2), -(sp)					;Save the address of the word that the node that r2 points to in stack.
						mov r2, -(sp)						;Save the address of the node that r2 points to in stack.
						add #4, r2							;Move r2 to point on the next register.
						jsr pc, recRevBubble				;Jump again to recursive bubble sort.
					
						mov (sp)+, r2						;Get from stack into r2 register address of node.
						mov (sp)+, (r2)						;r2 points to the word in the node.
						mov r2, -(sp)						;Save the address of the node that r2 points to in stack.			
						add #2, r2							;Add 2 to r2
						mov (r2), -(sp)						;Save the word the node points to into stack.
						tst -(sp)							;Save slot in stack for output of wordsComapre.
						jsr pc, wordsCompare				;Jump to wordsCompare routine
						cmp #1, (sp)						;If wordsComapre returned 1 jump to swap.
						beq swap

beforefinish:			tst (sp)+							;Increase stack pointer.
						tst (sp)+							;Increase stack pointer.
						tst (sp)+							;Increase stack pointer.
finish_revBubbleRec:	rts pc

swap:					sub #2, r2							;Reached here if swap is needed between two strings. Move r2 to point on the word before the node.
						mov (r2), r5						;r5 is saving temporarily the value of the beginning of sring.
						mov @2(r2), (r2)					;Move the string that the node of the current string points to, to the current string place.
						mov r5, @2(r2)						;Move the current string to the place of the second string.
						jmp beforefinish					;Jump to end of routine.
					
;end of revBubble function

;beginnig of wordsCompare function
;						
;wordsCompare function receives addresses of two strings from stack, and compares them.
;
;The register used to branch to the function is pc.
;Input Parameters:	stack - addresses of two strings.
;
;Output: 			stack - 0 if the are equal, 1 if the first string is bigger, 2 if the second string is bigger.
;
;

wordsCompare:			mov r1, -(sp)						;Store register in stack.
						mov r2, -(sp)						;Store register in stack.
						mov 12(sp), r1						;Assigning first input string to r1 register from stack.
						mov 10(sp), r2						;Assigning second input string to r2 register from stack.
						mov r3, -(sp)						;Store register in stack.
						mov r4, -(sp)						;Store register in stack.
						mov (r1), r3						;Save the first char of word in register
						mov (r2), r4						;Save the first char of word in register
			
loop_WordsCompare:		cmpb #'@, @(r1)						;Check if the first string reached the end.
						beq WC_checkFirstEnd				;If yes, jump to WC_checkFirstEnd.
						cmpb #'@, @(r2)						;Check if the second string reached the end.
						beq FirstBig_WordsCompare			;If yes, jump to FirstBig_WordsCompare.
						cmpb @(r1), @(r2)					;Check if the current char in both strings are equals.
						beq WC_nextChar						;If yes, jump to WC_nextChar.
			
						jmp notEqual_WordsCompare			;If the first string didnt reach the end, and the current
															;char of the strings are not equals jump to notEqual_WordsCompare.
			
WC_nextChar:			inc (r1)							;Reached here if the current char of both string are equals. Move to 
						inc (r2)							;the next char of both strings.
						jmp loop_WordsCompare				;Jump back to loop to compare the next chars.
			
notEqual_WordsCompare:	cmpb @(r1), @(r2)					;Reached here if current chars of both words are not equals. If the first word's char is bigger
						bgt FirstBig_WordsCompare			;than jump to FirstBig_WordsCompare, if not jump to SecondBig_WordsCompare.
						jmp SecondBig_WordsCompare
						
WC_checkFirstEnd:		cmpb #'@, @(r2)						;Reached here if the first string reached the end. Check if the second string reached the end too.
						beq equal_WordsCompare				;If yes, jump to equal_WordsCompare.
						
SecondBig_WordsCompare:	mov #2, 12(sp)						;Reached here if second string is bigger. Move 2 to the output in the stack and
						jmp finish_WordsCompare				;go to finish_WordsCompare.
						
FirstBig_WordsCompare:	mov #1, 12(sp)						;Reached here if first string is bigger. Move 1 to the output in the stack and
						jmp finish_WordsCompare				;go to finish_WordsCompare.
						
equal_WordsCompare:		mov #0, 12(sp)						;Reached here if the string are equals, move 0 to the output in the stack and go to finish_WordsCompare.

finish_WordsCompare:	mov r3, (r1)						;Move back the address to the first char of the string.
						mov r4, (r2)						;Move back the address to the first char of the string.
						mov (sp)+, r4						;Return register's original value to register.
						mov (sp)+, r3						;Return register's original value to register.
						mov (sp)+, r2						;Return register's original value to register.
						mov (sp)+, r1						;Return register's original value to register.
						rts pc

;end of wordsCompare function

;;;;;;;global parameters
intro_flag:   .word 0
;subr_lst: .word sub0, sub1, sub2, sub3, sub4, sub5
end_line_flag:		.word 0
space:				.word 40, '@
arrow:				.word 76, '@
endl1:              .word 12, '@
endl2:              .word 15, '@
backspc:            .word 10, '@
retry:              .word 0
len_msg: 			.ascii <Please enter length of the message:@>
time_msg: 			.ascii <Please enter number of seconds to finish the message:@>
start1_msg: 		.ascii <You can start to write your message.@>
start2_msg:			.ascii <Don't worry we'll assist you!@>
fix1_msg: 			.ascii <Do you mean'@>
fix2_msg: 			.ascii <'y/n?@>
no1_fix:   			.ascii <Sorry '@>
no2_fix:   			.ascii <' not in our dictionary!@>
add1:     			.ascii <'@>
add2:     			.ascii <' was added to the message!@>
print1_sent: 		.ascii <The message is: "@>
print2_sent: 		.ascii <".@>
time1: 				.ascii <Time left: @>
time2: 				.ascii < seconds@>
finish1: 			.ascii <Thank you!@>
finish2: 			.ascii <Bye Bye!@>
timeOut: 			.ascii <Sorry Time is up!@>
no1_match: 			.ascii <Sorry '@>
no2_match: 			.ascii <' has no other suggestions in our dictionary!@> 

.even
flag_nw:     .word 0
pointer_hist: .word  words_hist
words_hist:  .blkw 50
space_flag:     .word    2
blink_flag:      .word   0
err:  .word 0
end_prog:     .word 0


msg_len:      .ascii <@00@>
.even
len_ptr:      .blkw   1
ready_input:  .word 0
sentence:    .blkw  165
sentp:       .word sentence
sentSt:      .word sentence
prt_buf:      .word 0
timep:     .blkw 1
prn_time:        .blkw 1
time:            .ascii   <@0000@>
.even

.even
busy:     .word 0
wordStart: .word words
words:	   .word print1
print1:     .blkw 30





count:      .word 0
countcp:    .word 0

.even
len:   				.blkw 1
sent1:  			.blkw 30

time_counter:		.word 0
word_count:  		.blkw 50
buffer_pointer:		.word buffer
buffer:				.ascii <@@@@@@@@@@>
buffer_add:			.word buffer
input_buffer:		.ascii <4@>
copy_buffer:		.word 0
pointer:	.word pointerStart
pointerStart:    .blkw 1
is_input:			.word 0
endl:                .ascii < @>
word_cp:              .blkw 1
buf_clr:		  .word 0
CR_dict:     .blkw 1

