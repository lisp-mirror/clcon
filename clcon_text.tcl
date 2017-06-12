# ::clcon_text::clcon_text Widget based on example from snit FAQ
# NEVER USE PUTS or SHOWVAR in this file: it calls update so everything breaks
# Options:
#   -readonly
#   -send_to_lisp
# Subcommands
#   pathName Freeze
#   pathName Unfreeze
#
# If -readonly option == 1, this makes text widget readonly,
# keeping an ability to receive focus
# and a visible cursor for keyboard navigation.
# Readonly text
# text can still be altered with
# RoInsert, RoDelete, RoReplace
#
# If -readonly option == 0, widget can be operated normally.
# RoInsert, RoDelete, RoReplace subprocedures still work. (or how
#
#
# If -send_to_lisp options is set, all text modifications are
# sent to lisp. Do not change this option after editing startup.
#
# Freeze/unfreeze - utility for synchronous buffer modifications.
#
# pathName Freeze
#
#  - makes widget to collect all events it receives instead of processing them
# pathName Unfreeze
#
#  - makes event to process all delayed event.
# Denis Budyak 2015-2017
# #include <MIT License>

package require Tk
package require snit
# package require ctext

# 444555 
# Здесь был обходной путь для ошибки ввода с клавиатуры, см. тупиковые ветки в районе 2016-09-24. Похоже, что она будет решена в Tk и он больше не нужен.
# На момент написания у нас работает патченная отладочная версия wish86tg.exe, 
# где эта проблема решена.
# Для логгирования этих событий вот это вставить в
# нижеследующий биндинг для <KeyPress>
# putd [string cat = %k = %K = %N = %s = <<< %A >>>]

# Мы поменяли исходное сообщение из ../../tcl-8.6.6/lib/tk8.6/text.tcl 
bind Text <KeyPress> {
	tk::TextInsert %W %A
}

# FIXME some options might be buffer-related, not text-related.
namespace eval ::clcon_text {

    ::snit::type opened_file {
        option -filename {}
        # we can distinguish existing file from new one by -filemtime:
        # for new files it is empty
        option -filemtime {}
    }
    
    variable GlobalPendingText2OduEventCounter
    if {![info exists GlobalPendingText2OduEventCounter]} {
        set GlobalPendingText2OduEventCounter 0
    }
    
    ::snit::widgetadaptor clcon_text {
        option -readonly -default 0
        # Send all editions to lisp
        option -send_to_lisp -default 0
        # Input is put into a special queue instead of applying to widget
        # This might be instance variable, but we need it in wrappers
        # This variable has three states: 0=normal,1=freezed,2=unfreezing
        option -private_freezed -default 0
        # Levels of nested freezing
        option -private_freeze_level 0 
        option -private_freezed_events_queue [list]
        # PRIVATE. Number of modifications sent which were not processed by oduvanchik yet
        option -private_pending_sent_modifications 0
        option -private_pending_far_tcl_continuations 0
        option -opened_file {}
        # PRIVATE. Глобальный счётчик изменений
        option -tick_count 0
        # Список tick_count-ов, которые мы запрашивали у лиспа, по кодам слоёв
        option -tick_count-когда-перекрашивали {-1}
        # Раскрашивать ли латиницу 1 - да, 0 - нет.
        option -раскрашивать_ли_латиницу 0
        constructor {args} {
            installhull using btext
            $self configure -opened_file [opened_file $self.opened_file]
            # Apply any options passed at creation time.
            $self configurelist $args
            # Set FreezableText tags at first place (maybe should have placed to other place)
            set w [$self RealText]
            set CurrentBindTags [bindtags $w]
            set NewBindTags [SubstituteSingleValueInListVarKeyEq CurrentBindTags Text FreezableText]
            # set NewBindTags2 [lappend $NewBindTags TextTrackCursor]
            bindtags $win $NewBindTags
            bindtags $w $NewBindTags
            bind $win <<ContinueUnfreeze>> "$self Unfreeze"
            bind $win <<UpdateFreezeIndicator>> "$self UpdateFreezeIndicator"
            bind $win <<UpdateFreezeIndicatorLater>> "after 500 event generate $self <<UpdateFreezeIndicator>>"
            ::edt::CreateHighlightTags $self
            ::clcon_text::Создать_тег_для_раскраски_латиницы $self
            $self tag raise sel

            global $self.StatusBarInfo
            set $self.StatusBarInfo(Mode) Lisp
            set $self.StatusBarInfo(CursorPos) {}
            set $self.StatusBarInfo(Package) {CL-USER}
            set $self.StatusBarInfo(Readtable) {NIL}
            set $self.StatusBarInfo(ConnectionStatus) {?}
            set $self.StatusBarInfo(Tick_count) {0}
        }
        destructor {
            $options(-opened_file) destroy
            global $self.StatusBarInfo
            array unset $self.StatusBarInfo
            DestroyBackendBuffer $win
        }

        # Maybe disable the text widget's insert and delete methods, to
        # make this readonly.
        method insert {args} { if {!$options(-readonly)} { $self RoInsert {*}$args }}
        method delete {args} { if {!$options(-readonly)} { $self RoDelete {*}$args }}
        method replace {args} { if {!$options(-readonly)} { $self RoReplace {*}$args }}

        # Returns real text widget (this is not $self in case of btext)
        method RealText {} {
            return $self.t
        }

        method incr_tick_count {} {
            global $self.StatusBarInfo
            set new_tick_count [expr $options(-tick_count) + 1 ]
            $self configure -tick_count $new_tick_count
            set $self.StatusBarInfo(Tick_count) $new_tick_count
        }
        
        # Enable synonyms, so the program can operate on readonly text
        method RoInsert {args} {
            putd "RoInsert $args"
            # Хороший вопрос - менять счётчик до или после отправки в лисп?
            $self incr_tick_count
            MaybeSendToLisp $self i $args
            set tags ""
            set вставляемый_текст [lindex $args 1]
            set вставляем_одну_букву [expr 1 == [string length ${вставляемый_текст}]]
            set раскрашивать_ли_латиницу $options(-раскрашивать_ли_латиницу)
            if {${раскрашивать_ли_латиницу}} {
                # Здесь некая проблема - у нас много разновидностей команды insert, в т.ч. вставляющей более одной буквы. 
                # Нам надо вставить теги для латиницы. Непонятно, как определить диапазон, к-рый мы только что вставили. Поэтому
                # мы рассматриваем частный случай, когда вставляется только одна буква - тут мы можем определить тег. В остальных случаях
                # мы просто пересоздаём теги для всего буфера
                set вставляем_одну_латинскую_букву [expr ${вставляем_одну_букву} && [string match {[a-zA-Z]} ${вставляемый_текст}]]
                putd "${вставляемый_текст} ${вставляем_одну_букву} ${раскрашивать_ли_латиницу} ${вставляем_одну_латинскую_букву}"
                if {${вставляем_одну_латинскую_букву}} {
                    set tags "vyd_lat"
                }
            }
            putd "$hull insert {*}$args $tags"
            set result [$hull insert {*}$args $tags]
            if {!${вставляем_одну_букву} && ${раскрашивать_ли_латиницу}} {
              Включить_выключить_раскраску_латиницы $self 1
            }
            
            after idle [list ::edt::ПопроситьЛиспПрислатьДанныеОРаскраске $self 0]
            return $result
        }
        method RoDelete {args} {
            putd "RoDelete $args"
            $self incr_tick_count
            MaybeSendToLisp $self d $args
            set result [$hull delete {*}$args]
            after idle [list ::edt::ПопроситьЛиспПрислатьДанныеОРаскраске $self 0]
            return $result
        }
        method RoReplace {args} {
            error "До сих пор команда RoReplace была не нужна - что-то не так!"
        }
        
        # NSL stands for "not send to lisp". Specially for oduvanchik commands which
        # do text editings, see do-editing-on-tcl-side.lisp
        # Однако номер состояния они увеличивают!
        method RoInsertNSL {args} {
            putd "RoInsertNSL $args"
            $self incr_tick_count
            $hull insert {*}$args
        }
        method RoDeleteNSL {args} {
            putd "RoDeleteNSL $args"
            $self incr_tick_count
            $hull delete {*}$args
        }
        # It looks like we don't need Replace at all
        
        method UsesLispP {} {
            variable ::tkcon::OPT
            return [expr {$OPT(oduvan-backend) && [$self cget -send_to_lisp]}]
        }
        
        method RememberEvent {script} {
            set q $options(-private_freezed_events_queue)
            # putd "444444 Remembering script $script"
            lappend q $script
            $self configure -private_freezed_events_queue $q
            #putd "[llength $q] events remembered]" 
        }

        method Freeze {} {
            # ::mprs::AssertEq $options(-private_freezed) 0 "Freeze: must be unfreezed"
            set old $options(-private_freezed)
            switch -exact $old {
                0 {
                    putd "Called Freeze from 0. Will freeze"
                }
                1 { error "We need multi-level freezing" }
                2 {
                    putd "I guess we called Freeze from Unfreeze. Lets freeze again"
                }
            }
            $self configure \
                -private_freezed 1 \
                -private_freeze_level [expr {1 + $options(-private_freeze_level)}]
            event generate $self <<UpdateFreezeIndicatorLater>>
        }


        method UpdateFreezeIndicator {} {
            if {![winfo exists $self]} {
                return
            }
            global $self.StatusBarInfo
            if { $options(-private_freezed) } {
                set new_status "FROZEN"
            } elseif { [$self UsesLispP] }  { 
                set new_status "connected"
            } else {
                set new_status {} 
            }
            set $self.StatusBarInfo(ConnectionStatus) $new_status
        }

        
        method ResetBackendBuffer {} {
            if {![$self UsesLispP]} {
                tk_messageBox -message "$self не использует UsesLispP. Не могу пересоздать буфер одуванчика"
                return 
            }
            DestroyBackendBuffer $self 
            $self configure -private_freezed_events_queue {}
            $self configure -private_freeze_level 0
            $self configure -private_freezed 0
            ConstructBackendBuffer $self
            MaybeSendToLisp $self ResendEntireTextToReconstructedBackendBuffer {}
        }

        method Unfreeze {} {
            putd "Entering Unfreeze"
            if {!$options(-send_to_lisp)} {
                return
            }
            set pfl -private_freeze_level
            switch -exact $options($pfl) {
                0 { putd "Unfreeze: error - must be freezed" }
                1 {
                    if {$options($pfl)==1} {
                        $self configure -private_freezed 2
                        $self ContinueUnfreeze
                    }
                }
                default {
                    $self configure $pfl [expr {$options($pfl) - 1}]
                }
            }
            event generate $self <<UpdateFreezeIndicatorLater>>
        }

        method ContinueUnfreeze {} {
            #putd "Entering ContinueUnfreeze"
            if {!$options(-send_to_lisp)} {
                return
            }
            set pfl -private_freeze_level
            set current_freezed_state $options(-private_freezed)
            if {$options($pfl)>1} {
                # Someone have refrozen us. Do not unfreeze anymore, but decrease locking level                
                $self configure -private_freezed 1 \
                    $pfl [expr {$options($pfl) - 1}]
                return
            }
            switch -exact $current_freezed_state {
                0 {
                    puts stderr "Losing queued events: $options(-private_freezed_events_queue)"
                    $self configure -private_freezed_events_queue {}
                    error "ContinueUnfreeze: expected -private_freezed = 0 or 2. All queued events will be lost"
                }
                1 {
                    # do nothing - we are in freezed state again
                }
                2 {
                    set q $options(-private_freezed_events_queue)
                    if {[llength $q]} {
                        set script [lindex $q 0]
                        set q [lrange $q 1 end]
                        $self configure -private_freezed_events_queue $q
                        if {$script ne {}} {
                            # putd "444444 Unfreeze: about to eval $script"
                            catch {eval $script} code
                            if {$code ne {}} { putd "Error when unfreezing, will try to proceed: $code" }
                            # Check-the-world
                            if {!$options(-send_to_lisp)} {
                                return
                            }
                        }
                        #putd "Now will shedule ContinueUnfreeze"
                        after 50 event generate $win <<ContinueUnfreeze>>
                    } else {
                        # Event queue empty, state is 2. just check that level is 1 and exit
                        ::mprs::AssertEq 1 $options($pfl)
                        $self configure -private_freezed 0 $pfl [expr {$options($pfl) - 1}]
                        # looks like we will unfreeze now. Update indicator
                        event generate $self <<UpdateFreezeIndicator>>
                        after idle [list ::clcon_text::tncm $self]
                    }
                }
            }
            event generate $self <<UpdateFreezeIndicatorLater>>
        }

        
        # Pass all other methods and options to the real text widget, so
        # that the remaining behavior is as expected.
        delegate method * to hull
        delegate option * to hull
    }

    proc lq {x} {
        ::tkcon::QuoteLispObjToString $x
    }
    
    proc ConstructBackendBuffer {clcon_text} {
        variable ::tkcon::OPT

        # If we want oduvan-backend, but there is no connection,
        # We won't try to communicate to lisp.
        # If later we connect to swank, the buffer will remain lame. 
        if {$::tkcon::OPT(oduvan-backend) && [::swcnn::SwankConnectedP]} {

            # this can be uncommented for debugging of the editor.
            # only first buffer's command are sent to lisp so that
            # less mess oduvanchik's state
            #if {[::edt::Bi2btext "buf1"] ne $clcon_text} {
            #   return
            #}

            $clcon_text configure -send_to_lisp 1
            MaybeSendToLisp $clcon_text ConstructBackendBuffer {}
        }
        event generate $clcon_text <<UpdateFreezeIndicator>>
    }

    proc DestroyBackendBuffer {clcon_text} {
        MaybeSendToLisp $clcon_text DestroyBackendBuffer {} {} 1
        event generate $clcon_text <<UpdateFreezeIndicator>>
    }

    # Note that we have sent notification, or if notification
    # was processed on lisp side. Control number of pending events.
    # Args
    # increment - change of value
    # clcon_text - pathName of clcon_text widget
    # UseGlobalCounter - if 1, use global. If 0 - local. 
    proc IncrPendingSentNotifications {increment clcon_text UseGlobalCounter} {
        if {$UseGlobalCounter} {
            variable GlobalPendingText2OduEventCounter 
            incr GlobalPendingText2OduEventCounter $increment
            if {$GlobalPendingText2OduEventCounter>1} {
                # If we show this, something seem to be wrong
                showVarPutd GlobalPendingText2OduEventCounter
                putd "GlobalPendingText2OduEventCounter should be 0 or 1"
            }
            return $GlobalPendingText2OduEventCounter
        } else {
            if {![winfo exists $clcon_text]} {
                return 0
            }
            set j [$clcon_text cget -private_pending_sent_modifications]
            incr j $increment
            $clcon_text configure -private_pending_sent_modifications $j
            if {$j>1} {
                #putd "-private_pending_sent_modifications = $j"
            }
            return $j
        }
    }

    proc IncrPendingFarTclContinuations {increment clcon_text} {
        set j [$clcon_text cget -private_pending_far_tcl_continuations]
        incr j $increment
        $clcon_text configure -private_pending_far_tcl_continuations $j
        if {$j>1} {
            #putd "-private_pending_far_tcl_continuations = $j"
        }
        return $j
    }
    

    # ::clcon_text::MaybeSendToLisp
    # Sends to lisp an event reflecting changes in text widget, or requesting
    # to eval code on oduvanchik's side, or requesting to construct or destroy
    # backend buffer.
    # Args
    # clcon_text - pathname of clcon_text widget
    # type - type of event, "i", "d", "ConstructBackendBuffer", "DestroyBackendBuffer"
    # tick_count - значение опции -tick_count (число изменений с момента создания буфера)
    # arglist - list of arguments of event (see code)
    # UseGlobalPendingText2OduEventCounter - if 1, this is not buffer-specific event
    # (destroy event in fact)
    proc MaybeSendToLisp {clcon_text type arglist {far_tcl_continuation_body {}} {UseGlobalPendingText2OduEventCounter 0}} {
        variable ::tkcon::OPT
        
        # This is a temporary solution. This is a separate option indeed
        set qId [lq $clcon_text]
        switch -exact $type {
            n {
                # see ::clcon_text::tncm
                if {![$clcon_text UsesLispP]} {
                    return
                }
                # When unfreezing, we DO NOT send it. ContinueUnfreeze would send it for us
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set tick_count [$clcon_text cget -tick_count]
                set qIndex [::text2odu::CoerceIndex $clcon_text insert]
                set lispCmd "(clco:ncm $qId $tick_count $qIndex)"
            }
            h { 
                if {![$clcon_text UsesLispP]} {
                    return
                }
                # When unfreezing, we DO NOT send it. ContinueUnfreeze would send it for us
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set tick_count [$clcon_text cget -tick_count]
                set Слой [lindex $arglist 0]
                set lispCmd "(clco:nhi $qId $tick_count ${Слой})"
            }
            i {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set tick_count [$clcon_text cget -tick_count]
                set index [lindex $arglist 0]
                set qIndex [::text2odu::CoerceIndex $clcon_text $index]
                set qText [lq [lindex $arglist 1]]
                if {[lindex $arglist 2] ne ""} {
                    puts "При вставке текста неожиданный агумент. Все аргументы: $clcon_text $arglist"
                }
                set lispCmd "(clco:nti $qId $tick_count $qIndex $qText)"
            }
            d {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                if {[$clcon_text cget -private_freezed]} {
                    return
                }
                set tick_count [$clcon_text cget -tick_count]
                set b [lindex $arglist 0]
                set qB [::text2odu::CoerceIndex $clcon_text $b]
                set e [lindex $arglist 1]
                set qE [::text2odu::CoerceIndex $clcon_text $e]
                set lispCmd "(clco:notify-oduvan-tcl-text-delete $qId $tick_count $qB $qE)"
            }
            ResendEntireTextToReconstructedBackendBuffer {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                set tick_count [$clcon_text cget -tick_count]
                set qIndex [::text2odu::CoerceIndex $clcon_text 1.0]
                # I forgot this magic with end-1c. ::edt::DoSaveFile can spill the light...
                set qText [lq [$clcon_text get 1.0 end-1c]]
                set lispCmd "(clco:nti $qId $tick_count $qIndex $qText)"
            }
            ConstructBackendBuffer {
                if {![$clcon_text UsesLispP]} {
                    return
                }
                # FIXME clcon_text_to_file_name must be in this namespace. 
                lassign [::edt::clcon_text_to_file_name $clcon_text] FileName Reason
                set tick_count [$clcon_text cget -tick_count]
                set qFileName [lq $FileName]
                set lispCmd "(clco:notify-oduvan-construct-backend-buffer $qId $tick_count $qFileName)"
            }
            DestroyBackendBuffer {
                # We don't check if current buffer uses lisp, as it can not exist now already. But we at least should check is we have lisp at all.
                if {$OPT(oduvan-backend)} {
                    return
                }
                set lispCmd "(clco:notify-oduvan-destroy-backend-buffer $qId)"
            }
            CallOduvanchikFunction {
                if {![$clcon_text UsesLispP]} {
                    event generate $clcon_text <<UpdateFreezeIndicator>>
                    return
                }
                set tick_count [$clcon_text cget -tick_count]
                set qB [::text2odu::CoerceIndex $clcon_text insert]
                set FnAndArgs [lindex $arglist 0]
                set qOptions [::tkcon::QuoteTclListOfStringsForLisp [lindex $arglist 1]] 

                if {$far_tcl_continuation_body ne {}} {
                    ::clcon_text::IncrPendingFarTclContinuations 1 $clcon_text
                    set far_tcl_continuation_body [subst -nocommands {
                        $far_tcl_continuation_body
                        ::clcon_text::IncrPendingFarTclContinuations -1 $clcon_text
                    }]
                }

                # set qC [lq $far_tcl_continuation_body]
                set far_tcl_cont_id [::tkcon::GenContinuationCounter]
                ::mprs::EnqueueContinuation $far_tcl_cont_id $far_tcl_continuation_body
                set lispCmd "(clco:call-oduvanchik-function-with-clcon_text $qId $tick_count $qB $far_tcl_cont_id '($FnAndArgs) '($qOptions))"
            }
            default {
                error "Hurray! We found replace command $type with args $clcon_text $type $arglist!"
            }
        }

        IncrPendingSentNotifications 1 $clcon_text $UseGlobalPendingText2OduEventCounter
        set continuation [subst -nocommands {
            ::clcon_text::IncrPendingSentNotifications \
                -1 $clcon_text $UseGlobalPendingText2OduEventCounter
        }]
        # putd "632017 MaybeSendToLisp: about to send $lispCmd with cont $continuation"
        ::tkcon::EvalInSwankAsync $lispCmd $continuation ${::tkcon::find-existing}
    }

    # This is "static" protection. And what if script calls function which returns code?
    # It can also return numeric value of a code, in which case we fail. 
    proc CheckIfScriptDoesNotContainBreakContinueOrReturn {script} {
        if {[string match *break* $script]} {
            error "script contains break: $script"
        }
        if {[string match *continue* $script]} {
            error "script contains continue: $script"
        }
        if {[string match *return* $script]} {
            error "script contains return: $script"
        }
    }
    
    # In freezable text, all event handler scripts must be processed
    # with this function to support freezing protocol.
    # Optional destination is required when event is bound not to
    # text itself, but, say, to menu bar. In this case destination
    # must expand to pathName of text.
    # Note: macro is not hygienic, $W can be captured, this is why
    # it is protected with namespace prefix.
    # If you need continue, add another named arg.
    proc WrapEventScriptForFreezedText {script args} {
        named_args $args {-destination "%W" -add-break 0 -note-cursor-motion 1}
        CheckIfScriptDoesNotContainBreakContinueOrReturn $script 
        set Template [lindex \
            {"if {[<<<<destination>>>> cget -private_freezed]} {
   <<<<destination>>>> RememberEvent {<<<<OldEventBody>>>>}
 } else {
   <<<<OldEventBody>>>><<<<NoteCursorMotion>>>>
 }<<<<MaybeBreak>>>>"} 0]

   # Inser this into script     
   # putd 444444
   # putd {<<<<     OldEventBody      >>>>}
        
        if {$(-note-cursor-motion)} {
            set NoteCursorMotion  "
 after idle [list ::clcon_text::tncm <<<<destination>>>>]"
        } else {
            set NoteCursorMotion ""
        }        
        if {$(-add-break)} {
            set MaybeBreak " 
 break"
        } else {
            set MaybeBreak ""
        }
        set t0 [regsub -all <<<<NoteCursorMotion>>>> $Template $NoteCursorMotion]
        set t1 [regsub -all <<<<OldEventBody>>>> $t0 $script]
        set t2 [regsub -all <<<<destination>>>> $t1 $(-destination)]
        set t3 [regsub -all <<<<MaybeBreak>>>> $t2 $MaybeBreak]
        # putd cannot be used here as this is called too early
        # puts stderr "WrapEventScriptForFreezedText234324: $t3"
        return $t3
    }

    # Try to arrange things so that correct and minimal set of cursor position
    # changes were noted. 
    proc DoesEventModifyCursorPos {ev} {
        # In tk_samples/original-tkcon.tcl, special bindtag is used for that
        # We found that for keyboard we will not achieve that bindtag. But
        # if our keyboard shortcuts call oduvanchik functions, we anyway note
        # cursor position change on unfreeze.
        # Maybe using DoesEventModifyCursorPos is a better approach, but
        # we have other problems now and can not afford resources to implement it.
        # FIXME
    }

    proc WrapFreezableHandlerScript {ev} {
        set script [bind Text $ev]
        set script2 [WrapEventScriptForFreezedText $script]
        bind FreezableText $ev $script2
    }
   
    ## В Linux недоназначаются кнопки с KP_. Часть мы назначаем здесь. См. также fix_kp_in_linux.tcl 
    proc SetKPBindingsForText {} {
       if {[tk windowingsystem] eq "x11"} {

           foreach { KP Norm } { \
                              <KP_Prior> <Prior> <KP_Next> <Next> \ 
#                              <KP_Delete> <Delete> <KP_Insert> <Insert> \
#                             <KP_Home> <Home> <KP_End> <End> \
#                             <KP_Up> <Up> <KP_Down> <Down> \
#                             <KP_Left> <Left> <KP_Right> <Right> \ 
                             <Control-Key-KP_Home> <Control-Key-Home> \
                             <Control-Key-KP_End> <Control-Key-End>   \
                             <Control-Key-KP_Prior> <Control-Key-Prior> \
                             <Control-Key-KP_Next> <Control-Key-Next> \
                                     } {
              ::clcon_key::b bind Text $KP [ bind Text $Norm ]
           }

    ## Это скопировано из исходников text.tcl. Почему его пришлось сюда писать - я не знаю. Возможно, оно где-то стирается?
           bind Text <Control-Home> {
              tk::TextSetCursor %W 1.0
           }
           bind Text <Control-Shift-Home> {
              tk::TextKeySelect %W 1.0 
           }
           bind Text <Control-End> {
             tk::TextSetCursor %W {end - 1 indices}
           }
           bind Text <Control-Shift-End> {
              tk::TextKeySelect %W {end - 1 indices} 
           }
           # И дублируем для KP
           bind Text <Control-KP_Home> {
              tk::TextSetCursor %W 1.0
           }
           bind Text <Control-Shift-KP_Home> {
              tk::TextKeySelect %W 1.0 
           }
           bind Text <Control-KP_End> {
              tk::TextSetCursor %W {end - 1 indices}
           }
           bind Text <Control-Shift-KP_End> {
              tk::TextKeySelect %W {end - 1 indices}
           }
       }
    }
    

    proc SetCyrBindingsForText {} {
        bind Text <Control-Key-Z> {}
        bind Text <Control-Z> {}
        ::clcon_key::b bind Text <Control-Key-z> {catch { %W edit undo }}
        ::clcon_key::b bind Text <Control-Key-y> {catch { %W edit redo }}
        # there was something wrong with tk_strictMotif
        ::clcon_key::b bind Text <Control-k> {
            if {[%W compare end != insert+1c]} {
              if {[%W compare insert == {insert lineend}]} {
                %W delete insert
              } else {
                %W delete insert {insert lineend}
              }
            }
        }
    }

    # Fills FreezableText bindtag with wrapped bindings of Text
    # Tab and Shift-Tab bindings are bypassed - we do not like tabs in program code.
    proc InitBindingsOfFreezableText {} {
        foreach ev [bind Text] {
            if {[lsearch -exact {<Shift-Key-Tab> <Key-Tab>} $ev] != -1} {
                # Do nothing - these handlers contain "break" and they are not need as Tab will be overrided anyway
            } else {
                WrapFreezableHandlerScript $ev
            }
        }
        return
    }


    proc SubstituteSingleValueInListVarKeyEq {listVariable old new} {
        upvar 1 $listVariable var
        set idx [lsearch -exact $var $old]
        if {$idx >= 0} {
            set var [lreplace $var $idx $idx [list $new]]
        }
    }

    # UserContBody is a body of procedure to be called with two parameters: clcon_text and EventAsList. If supplied, it must call Unfreeze. 
    # OduvanchikFunctionNameAndArgs represents function from :oduvanchik home-package and its arguments, in a readable form. All packages to symbols must be specified. All string must be quoted with ::tkcon::QuoteTclStringForLisp. Subforms are not evaluated. Consider it as aquote list without outermost '()
    # Elements of list are passed to funcall. 
    # Options is a dict of options. Current known options are:
    # send_selection : if 1, selection is sent to odu at the beginning of command
    # См. также "vyzvatq funkciyu zpt opredelyonnuyu ne v pakete oduvanchika"
    proc CallOduvanchikFunction {clcon_text OduvanchikFunctionNameAndArgs {UserContBody {}} {Options {}}} {
        variable ::tkcon::OPT
        if {![$clcon_text UsesLispP]} {
            error "Unable to call oduvanchik functions with oduvan-backend disabled (for this buffer)"
        }
        $clcon_text Freeze
        if {$UserContBody ne {}} {
            set ContBody [subst -nocommand {apply {{clcon_text EventAsList} $UserContBody} $clcon_text \$EventAsList}]
        } else {
            set ContBody [subst -nocommand {$clcon_text Unfreeze}]
        }
        showVarPutd ContBody
        MaybeSendToLisp $clcon_text CallOduvanchikFunction [list $OduvanchikFunctionNameAndArgs $Options] $ContBody
    }

    # Send cursor position to oduvanchik. This is for display purposes only, e.g. for
    # highlighting open parens. Normaly this function is programmed at after idle event.
    # When clcon_text is freezed, event is ignored. When unfreezing, it is send from
    # the unfreeze 2. 
    proc tncm {btext_or_btext_dot_t} {
        set x $btext_or_btext_dot_t
        # before idle came, widget could be destroyed.
        set clcon_text [::edt::CoerceTextToItsBText $x]
        if {![winfo exists $clcon_text]} {
            putd "Entered tncm:oops $clcon_text"
            return
        }
        global $clcon_text.StatusBarInfo
        set $clcon_text.StatusBarInfo(CursorPos) [$clcon_text index insert]
        MaybeSendToLisp $clcon_text n {}
    }

    # # To be called from oi::delete-region
    # proc clcon_text_delete_bb_ee {clcon_text} {
    #     putd stderr "Entered clcon_text_delete_bb_ee"
    #     $clcon_text delete bb ee
    #     $clcon_text mark unset bb
    #     $clcon_text mark unset ee
    #     putd stderr "About to exit clcon_text_delete_bb_ee"
    #     return {}
    # }

    # Oduvanchik is disconnected. Special care must be taken in the
    # case we have unfreezing processing on stack.
    proc OduvanchikDisconnected_clcon_text {clcon_text} {
        $clcon_text configure                          \
            -send_to_lisp 0                            \
            -private_freezed 0                         \
            -private_freeze_level 0                    \
            -private_freezed_events_queue [list]       \
            -private_pending_sent_modifications 0      \
            -private_pending_far_tcl_continuations 0
    }


    # returns path to a file, if there is one. Otherwise throws an error which you should process
    proc PathToAFile {clcon_text} {
        set opened_file [$clcon_text cget -opened_file]
        set filename [$opened_file cget -filename]
        return [file dirname $filename]
    }

    proc Создать_тег_для_раскраски_латиницы {clcon_text} {
        $clcon_text tag configure vyd_lat
        Включить_выключить_раскраску_латиницы $clcon_text [$clcon_text cget -раскрашивать_ли_латиницу]
    }

    # Второй параметр - включить или выключить. Если передать пустое значение 
    # для вкл_выкл, то оно станет противоположным текущему значению в виджете.
    proc Включить_выключить_раскраску_латиницы { clcon_text args }  {
        set вкл_выкл [lindex $args 0]
        if {${вкл_выкл} eq ""} {
            set вкл_выкл [expr 1 - [$clcon_text cget -раскрашивать_ли_латиницу]]
        }
        $clcon_text configure -раскрашивать_ли_латиницу ${вкл_выкл}
        # Правьмя наверняка в snit можно сделать остальные действия следствием configure 
        $clcon_text tag configure vyd_lat -underline ${вкл_выкл}
        # if {${вкл_выкл}} {
        #   $clcon_text tag configure vyd_lat -font {{Courier} -20 bold}
        #} else {
        #   $clcon_text tag configure vyd_lat -font {{Courier} -20}
        #}
        if {${вкл_выкл}} {
           foreach i [$clcon_text search -regexp -all {[a-zA-Z]} 0.0] { $clcon_text tag add vyd_lat $i }
        } else {
           $clcon_text tag remove vyd_lat 1.0 end
        }
    }

    proc Показать_экранную_клавиатуру {Приёмник} {
        set w .ЭкраннаяКлавиатура
        destroy $w 
        toplevel $w
        wm title $w "Экранная клавиатура для ${Приёмник}"
        bind $w <Escape> [list destroy $w]
        ::clcon_key::b bind $w <Control-Key-W> [list destroy $w]
        pack [::clcon::keyboard $w.kbd -title "Ввод значков для ${Приёмник}" -keys {0xD7 0xF7 0xD8 0x2116} -receiver ${Приёмник}]
        # - {0x410-0x44f} - кириллица
        ::gui_util::FocusWindowByName $w
        grab $w
    } 


    # InitOneBindingOfFreezableText <Key-Return>
    SetCyrBindingsForText

    # Под линуксом на ноутбуке странно работала цифровая клавиатура...
    SetKPBindingsForText

    InitBindingsOfFreezableText
}

######################## Example of readonly text ###############
# See test/freezing-test-2.tcl

################### Example of freezing ########################
# See test/freezing-test-1.tcl


