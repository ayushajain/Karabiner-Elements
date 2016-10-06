#pragma once

#include "boost_defs.hpp"

#include "event_dispatcher_manager.hpp"
#include "event_tap_manager.hpp"
#include "logger.hpp"
#include "manipulator.hpp"
#include "modifier_flag_manager.hpp"
#include "system_preferences.hpp"
#include "types.hpp"
#include <IOKit/hidsystem/ev_keymap.h>
#include <boost/optional.hpp>
#include <list>
#include <thread>
#include <unordered_map>

namespace manipulator {
class event_manipulator final {
public:
  event_manipulator(const event_manipulator&) = delete;

  event_manipulator(void) : event_dispatcher_manager_(),
                            event_source_(CGEventSourceCreate(kCGEventSourceStateHIDSystemState)),
                            modifier_flag_manager_(),
                            key_repeat_manager_(*this) {
                                
                                //=== takahasix ============================================
                                //=== takahasix ============================================
                                //=== takahasix ============================================
                                memset(counter_, 0, sizeof(counter_));

                                // For user, rewrite this config, if you need.
                                
                                ck2ck_[ComplexKey(false, false, true, false, false, false, false, krbn::key_code(15))] = ComplexKey(false, false, false, false, false, false, false, krbn::key_code::right_arrow);
                                ck2ck_[ComplexKey(false, false, true, false, false, false, false, krbn::key_code(11))] = ComplexKey(false, false, false, false, false, false, false, krbn::key_code::left_arrow);
                                ck2ck_[ComplexKey(false, false, true, false, false, false, false, krbn::key_code(14))] = ComplexKey(false, false, false, false, false, false, false, krbn::key_code::up_arrow);
                                ck2ck_[ComplexKey(false, false, true, false, false, false, false, krbn::key_code(13))] = ComplexKey(false, false, false, false, false, false, false, krbn::key_code::down_arrow);
                                
                                //=== takahasix ============================================
                                //=== takahasix ============================================
                                
  }

  ~event_manipulator(void) {
    event_tap_manager_ = nullptr;

    if (event_source_) {
      CFRelease(event_source_);
      event_source_ = nullptr;
    }
  }

  bool is_ready(void) {
    return event_dispatcher_manager_.is_connected() &&
           event_source_ != nullptr;
  }

  void grab_mouse_events(void) {
    event_tap_manager_ = std::make_unique<event_tap_manager>(modifier_flag_manager_);
  }

  void ungrab_mouse_events(void) {
    event_tap_manager_ = nullptr;
  }

  void reset(void) {
    key_repeat_manager_.stop();

    manipulated_keys_.clear();
    manipulated_fn_keys_.clear();

    modifier_flag_manager_.reset();
    modifier_flag_manager_.unlock();

    event_dispatcher_manager_.set_caps_lock_state(false);
  }

  void reset_modifier_flag_state(void) {
    modifier_flag_manager_.reset();
    // Do not call modifier_flag_manager_.unlock() here.
  }

  void relaunch_event_dispatcher(void) {
    event_dispatcher_manager_.relaunch();
  }

  void set_system_preferences_values(const system_preferences::values& values) {
    std::lock_guard<std::mutex> guard(system_preferences_values_mutex_);

    system_preferences_values_ = values;
  }

  void clear_simple_modifications(void) {
    simple_modifications_.clear();
  }

  void add_simple_modification(krbn::key_code from_key_code, krbn::key_code to_key_code) {
    simple_modifications_.add(from_key_code, to_key_code);
  }

  void clear_fn_function_keys(void) {
    fn_function_keys_.clear();
  }

  void add_fn_function_key(krbn::key_code from_key_code, krbn::key_code to_key_code) {
    fn_function_keys_.add(from_key_code, to_key_code);
  }

  void create_event_dispatcher_client(void) {
    event_dispatcher_manager_.create_event_dispatcher_client();
  }

  void handle_keyboard_event(device_registry_entry_id device_registry_entry_id, krbn::key_code from_key_code, bool pressed) {
    krbn::key_code to_key_code = from_key_code;

    // ----------------------------------------
    // modify keys
    if (!pressed) {
      if (auto key_code = manipulated_keys_.find(device_registry_entry_id, from_key_code)) {
        manipulated_keys_.remove(device_registry_entry_id, from_key_code);
        to_key_code = *key_code;
      }
    } else {
      if (auto key_code = simple_modifications_.get(from_key_code)) {
        manipulated_keys_.add(device_registry_entry_id, from_key_code, *key_code);
        to_key_code = *key_code;
      }
    }

    // ----------------------------------------
    // modify fn+arrow, function keys
    if (!pressed) {
      if (auto key_code = manipulated_fn_keys_.find(device_registry_entry_id, to_key_code)) {
        manipulated_fn_keys_.remove(device_registry_entry_id, to_key_code);
        to_key_code = *key_code;
      }
    } else {
      boost::optional<krbn::key_code> key_code;

      if (modifier_flag_manager_.pressed(krbn::modifier_flag::fn)) {
        switch (to_key_code) {
        case krbn::key_code::return_or_enter:
          key_code = krbn::key_code::keypad_enter;
          break;
        case krbn::key_code::delete_or_backspace:
          key_code = krbn::key_code::delete_forward;
          break;
        case krbn::key_code::right_arrow:
          key_code = krbn::key_code::end;
          break;
        case krbn::key_code::left_arrow:
          key_code = krbn::key_code::home;
          break;
        case krbn::key_code::down_arrow:
          key_code = krbn::key_code::page_down;
          break;
        case krbn::key_code::up_arrow:
          key_code = krbn::key_code::page_up;
          break;
        default:
          break;
        }
      }

      // f1-f12
      {
        auto key_code_value = static_cast<uint32_t>(to_key_code);
        if (kHIDUsage_KeyboardF1 <= key_code_value && key_code_value <= kHIDUsage_KeyboardF12) {
          bool keyboard_fn_state = false;
          {
            std::lock_guard<std::mutex> guard(system_preferences_values_mutex_);
            keyboard_fn_state = system_preferences_values_.get_keyboard_fn_state();
          }

          bool fn_pressed = modifier_flag_manager_.pressed(krbn::modifier_flag::fn);

          if ((fn_pressed && keyboard_fn_state) ||
              (!fn_pressed && !keyboard_fn_state)) {
            // change f1-f12 keys to media controls
            if (auto k = fn_function_keys_.get(to_key_code)) {
              key_code = *k;
            }
          }
        }
      }

      if (key_code) {
        manipulated_fn_keys_.add(device_registry_entry_id, to_key_code, *key_code);
        to_key_code = *key_code;
      }
    }
      
      
      //=== takahasix ============================================
      //=== takahasix ============================================
      //=== takahasix ============================================
      if(shikakari_){
          
          // Stop key_repeat_manager.
          key_repeat_manager_.stop();
          
          ComplexKey src = shikakariDst_;
          ComplexKey dst = shikakariSrc_;
          
          // Clear src's key.
          post_key(src.key_, src.key_, false, false);
          counter_[(uint32_t)src.key_]--;
          
          // Clear src's modifier.
          if(src.commandLeft_){
              post_modifier_flag_event(krbn::key_code(227), false);
              counter_[227]--;
          }
          if(src.commandRight_){
              post_modifier_flag_event(krbn::key_code(231), false);
              counter_[231]--;
          }
          if(src.option_){
              post_modifier_flag_event(krbn::key_code(226), false);
              counter_[226]--;
          }
          if(src.shiftLeft_){
              post_modifier_flag_event(krbn::key_code(225), false);
              counter_[225]--;
          }
          if(src.shiftRight_){
              post_modifier_flag_event(krbn::key_code(229), false);
              counter_[229]--;
          }
          if(src.control_){
              post_modifier_flag_event(krbn::key_code(224), false);
              counter_[224]--;
          }
          if(src.fn_){
              post_modifier_flag_event(krbn::key_code(4098), false);
              counter_[4098]--;
          }
          
          // Set dst's modifier.
          if(dst.commandLeft_){
              post_modifier_flag_event(krbn::key_code(227), true);
              counter_[227]++;
          }
          if(dst.commandRight_){
              post_modifier_flag_event(krbn::key_code(231), true);
              counter_[231]++;
          }
          if(dst.option_){
              post_modifier_flag_event(krbn::key_code(226), true);
              counter_[226]++;
          }
          if(dst.shiftLeft_){
              post_modifier_flag_event(krbn::key_code(225), true);
              counter_[225]++;
          }
          if(dst.shiftRight_){
              post_modifier_flag_event(krbn::key_code(229), true);
              counter_[229]++;
          }
          if(dst.control_){
              post_modifier_flag_event(krbn::key_code(224), true);
              counter_[224]++;
          }
          if(dst.fn_){
              post_modifier_flag_event(krbn::key_code(4098), true);
              counter_[4098]++;
          }
          
          shikakari_ = false;
      }
      
      
      if(pressed==true && krbn::types::get_modifier_flag(to_key_code)==krbn::modifier_flag::zero){
          ComplexKey ck(counter_, to_key_code);
          for(std::map<ComplexKey, ComplexKey>::iterator it=ck2ck_.begin(); it!=ck2ck_.end(); it++){
              ComplexKey src = it->first;
              ComplexKey dst = it->second;
              if(src==ck){
                  // Clear src's modifier.
                  if(src.commandLeft_){
                      post_modifier_flag_event(krbn::key_code(227), false);
                      counter_[227]--;
                  }
                  if(src.commandRight_){
                      post_modifier_flag_event(krbn::key_code(231), false);
                      counter_[231]--;
                  }
                  if(src.option_){
                      post_modifier_flag_event(krbn::key_code(226), false);
                      counter_[226]--;
                  }
                  if(src.shiftLeft_){
                      post_modifier_flag_event(krbn::key_code(225), false);
                      counter_[225]--;
                  }
                  if(src.shiftRight_){
                      post_modifier_flag_event(krbn::key_code(229), false);
                      counter_[229]--;
                  }
                  if(src.control_){
                      post_modifier_flag_event(krbn::key_code(224), false);
                      counter_[224]--;
                  }
                  if(src.fn_){
                      post_modifier_flag_event(krbn::key_code(4098), false);
                      counter_[4098]--;
                  }
                  
                  // Set dst's modifier.
                  if(dst.commandLeft_){
                      post_modifier_flag_event(krbn::key_code(227), true);
                      counter_[227]++;
                  }
                  if(dst.commandRight_){
                      post_modifier_flag_event(krbn::key_code(231), true);
                      counter_[231]++;
                  }
                  if(dst.option_){
                      post_modifier_flag_event(krbn::key_code(226), true);
                      counter_[226]++;
                  }
                  if(dst.shiftLeft_){
                      post_modifier_flag_event(krbn::key_code(225), true);
                      counter_[225]++;
                  }
                  if(dst.shiftRight_){
                      post_modifier_flag_event(krbn::key_code(229), true);
                      counter_[229]++;
                  }
                  if(dst.control_){
                      post_modifier_flag_event(krbn::key_code(224), true);
                      counter_[224]++;
                  }
                  if(dst.fn_){
                      post_modifier_flag_event(krbn::key_code(4098), true);
                      counter_[4098]++;
                  }
                  
                  // Replace to_key_code with dst's key.
                  to_key_code = dst.key_;
                  
                  shikakariSrc_ = src;
                  shikakariDst_ = dst;
                  shikakari_ = true;
                  
                  break;
              }
          }
      }
      
      
      if(pressed){
          counter_[(uint32_t)to_key_code]++;
      }else{
          if(counter_[(uint32_t)to_key_code]==0){
              return;
          }
          counter_[(uint32_t)to_key_code]--;
      }
      //=== takahasix ============================================
      //=== takahasix ============================================
      //=== takahasix ============================================


    // ----------------------------------------
    // Post input events to karabiner_event_dispatcher

    if (to_key_code == krbn::key_code::caps_lock) {
      if (pressed) {
        toggle_caps_lock_state();
        key_repeat_manager_.stop();
      }
      return;
    }

    if (post_modifier_flag_event(to_key_code, pressed)) {
      key_repeat_manager_.stop();
      return;
    }

    post_key(from_key_code, to_key_code, pressed, false);

    // set key repeat
    long initial_key_repeat_milliseconds = 0;
    long key_repeat_milliseconds = 0;
    {
      std::lock_guard<std::mutex> guard(system_preferences_values_mutex_);
      initial_key_repeat_milliseconds = system_preferences_values_.get_initial_key_repeat_milliseconds();
      key_repeat_milliseconds = system_preferences_values_.get_key_repeat_milliseconds();
    }

    key_repeat_manager_.start(from_key_code, to_key_code, pressed,
                              initial_key_repeat_milliseconds, key_repeat_milliseconds);
  }

  void stop_key_repeat(void) {
    key_repeat_manager_.stop();
  }

  void refresh_caps_lock_led(void) {
    event_dispatcher_manager_.refresh_caps_lock_led();
  }

private:
  class manipulated_keys final {
  public:
    manipulated_keys(const manipulated_keys&) = delete;

    manipulated_keys(void) {
    }

    void clear(void) {
      std::lock_guard<std::mutex> guard(mutex_);

      manipulated_keys_.clear();
    }

    void add(device_registry_entry_id device_registry_entry_id,
             krbn::key_code from_key_code,
             krbn::key_code to_key_code) {
      std::lock_guard<std::mutex> guard(mutex_);

      manipulated_keys_.push_back(manipulated_key(device_registry_entry_id, from_key_code, to_key_code));
    }

    boost::optional<krbn::key_code> find(device_registry_entry_id device_registry_entry_id,
                                         krbn::key_code from_key_code) {
      std::lock_guard<std::mutex> guard(mutex_);

      for (const auto& v : manipulated_keys_) {
        if (v.get_device_registry_entry_id() == device_registry_entry_id &&
            v.get_from_key_code() == from_key_code) {
          return v.get_to_key_code();
        }
      }
      return boost::none;
    }

    void remove(device_registry_entry_id device_registry_entry_id,
                krbn::key_code from_key_code) {
      std::lock_guard<std::mutex> guard(mutex_);

      manipulated_keys_.remove_if([&](const manipulated_key& v) {
        return v.get_device_registry_entry_id() == device_registry_entry_id &&
               v.get_from_key_code() == from_key_code;
      });
    }

  private:
    class manipulated_key final {
    public:
      manipulated_key(device_registry_entry_id device_registry_entry_id,
                      krbn::key_code from_key_code,
                      krbn::key_code to_key_code) : device_registry_entry_id_(device_registry_entry_id),
                                                    from_key_code_(from_key_code),
                                                    to_key_code_(to_key_code) {
      }

      device_registry_entry_id get_device_registry_entry_id(void) const { return device_registry_entry_id_; }
      krbn::key_code get_from_key_code(void) const { return from_key_code_; }
      krbn::key_code get_to_key_code(void) const { return to_key_code_; }

    private:
      device_registry_entry_id device_registry_entry_id_;
      krbn::key_code from_key_code_;
      krbn::key_code to_key_code_;
    };

    std::list<manipulated_key> manipulated_keys_;
    std::mutex mutex_;
  };

  class simple_modifications final {
  public:
    simple_modifications(const simple_modifications&) = delete;

    simple_modifications(void) {
    }

    void clear(void) {
      std::lock_guard<std::mutex> guard(mutex_);

      map_.clear();
    }

    void add(krbn::key_code from_key_code, krbn::key_code to_key_code) {
      std::lock_guard<std::mutex> guard(mutex_);

      map_[from_key_code] = to_key_code;
    }

    boost::optional<krbn::key_code> get(krbn::key_code from_key_code) {
      std::lock_guard<std::mutex> guard(mutex_);

      auto it = map_.find(from_key_code);
      if (it != map_.end()) {
        return it->second;
      }

      return boost::none;
    }

  private:
    std::unordered_map<krbn::key_code, krbn::key_code> map_;
    std::mutex mutex_;
  };

  class key_repeat_manager final {
  public:
    key_repeat_manager(const key_repeat_manager&) = delete;

    key_repeat_manager(event_manipulator& event_manipulator) : event_manipulator_(event_manipulator),
                                                               queue_(dispatch_queue_create(nullptr, nullptr)),
                                                               timer_(0) {
    }

    ~key_repeat_manager(void) {
      stop();

      dispatch_release(queue_);
    }

    void stop(void) {
      std::lock_guard<std::mutex> guard(mutex_);

      if (timer_) {
        dispatch_source_cancel(timer_);
        dispatch_release(timer_);
        timer_ = 0;
      }
    }

    void start(krbn::key_code from_key_code, krbn::key_code to_key_code, bool pressed,
               long initial_key_repeat_milliseconds, long key_repeat_milliseconds) {
      // stop key repeat before post key.
      if (pressed) {
        stop();
      } else {
        if (from_key_code_ && *from_key_code_ == from_key_code) {
          stop();
        }
      }

      // set key repeat
      if (pressed) {
        if (to_key_code == krbn::key_code::mute ||
            to_key_code == krbn::key_code::vk_consumer_play) {
          return;
        }

        timer_ = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, DISPATCH_TIMER_STRICT, queue_);
        if (timer_) {
          std::lock_guard<std::mutex> guard(mutex_);

          dispatch_source_set_timer(timer_,
                                    dispatch_time(DISPATCH_TIME_NOW, initial_key_repeat_milliseconds * NSEC_PER_MSEC),
                                    key_repeat_milliseconds * NSEC_PER_MSEC,
                                    0);
          dispatch_source_set_event_handler(timer_, ^{
            event_manipulator_.post_key(from_key_code, to_key_code, pressed, true);
          });
          dispatch_resume(timer_);
          from_key_code_ = from_key_code;
        }
      }
    }

  private:
    event_manipulator& event_manipulator_;

    dispatch_queue_t queue_;
    dispatch_source_t timer_;

    boost::optional<krbn::key_code> from_key_code_;
    std::mutex mutex_;
  };

  bool post_modifier_flag_event(krbn::key_code key_code, bool pressed) {
    auto operation = pressed ? manipulator::modifier_flag_manager::operation::increase : manipulator::modifier_flag_manager::operation::decrease;

    auto modifier_flag = krbn::types::get_modifier_flag(key_code);
    if (modifier_flag != krbn::modifier_flag::zero) {
      modifier_flag_manager_.manipulate(modifier_flag, operation);

      // We have to post modifier key event via CGEventPost for some apps (Microsoft Remote Desktop)
      if (event_source_) {
        if (auto cg_key = krbn::types::get_cg_key(key_code)) {
          if (auto event = CGEventCreateKeyboardEvent(event_source_, static_cast<CGKeyCode>(*cg_key), pressed)) {
            CGEventSetFlags(event, modifier_flag_manager_.get_cg_event_flags(CGEventGetFlags(event), key_code));
            CGEventPost(kCGHIDEventTap, event);
            CFRelease(event);
          }
        }
      }

      return true;
    }

    return false;
  }

  void toggle_caps_lock_state(void) {
    modifier_flag_manager_.manipulate(krbn::modifier_flag::caps_lock, modifier_flag_manager::operation::toggle_lock);
    if (modifier_flag_manager_.pressed(krbn::modifier_flag::caps_lock)) {
      event_dispatcher_manager_.set_caps_lock_state(true);
    } else {
      event_dispatcher_manager_.set_caps_lock_state(false);
    }
  }

  void post_key(krbn::key_code from_key_code, krbn::key_code to_key_code, bool pressed, bool repeat) {
    if (event_source_) {
      if (auto cg_key = krbn::types::get_cg_key(to_key_code)) {
        if (auto event = CGEventCreateKeyboardEvent(event_source_, static_cast<CGKeyCode>(*cg_key), pressed)) {
          CGEventSetFlags(event, modifier_flag_manager_.get_cg_event_flags(CGEventGetFlags(event), to_key_code));
          CGEventSetIntegerValueField(event, kCGKeyboardEventAutorepeat, repeat);
          CGEventPost(kCGHIDEventTap, event);
          CFRelease(event);
        }

      } else {
        auto hid_system_key = krbn::types::get_hid_system_key(to_key_code);
        auto hid_system_aux_control_button = krbn::types::get_hid_system_aux_control_button(to_key_code);
        if (hid_system_key || hid_system_aux_control_button) {
          auto event_type = pressed ? krbn::event_type::key_down : krbn::event_type::key_up;
          auto flags = modifier_flag_manager_.get_io_option_bits();
          event_dispatcher_manager_.post_key(to_key_code, event_type, flags, repeat);
        }
      }
    }
  }

  event_dispatcher_manager event_dispatcher_manager_;
  modifier_flag_manager modifier_flag_manager_;
  CGEventSourceRef event_source_;
  key_repeat_manager key_repeat_manager_;
  std::unique_ptr<event_tap_manager> event_tap_manager_;

  system_preferences::values system_preferences_values_;
  std::mutex system_preferences_values_mutex_;

  simple_modifications simple_modifications_;
  simple_modifications fn_function_keys_;

  manipulated_keys manipulated_keys_;
  manipulated_keys manipulated_fn_keys_;
    
    //=== takahasix ============================================
    //=== takahasix ============================================
    //=== takahasix ============================================
    class ComplexKey final {
    public:
        ComplexKey(){
        }
        
        ComplexKey(bool commandLeft, bool commandRight, bool option, bool shiftLeft, bool shiftRight, bool control, bool fn, krbn::key_code key){
            commandRight_ = commandRight;
            commandLeft_ = commandLeft;
            option_ = option;
            shiftLeft_ = shiftLeft;
            shiftRight_ = shiftRight;
            control_ = control;
            fn_ = fn;
            key_ = key;
        }
        
        ComplexKey(int* counter, krbn::key_code key){
            commandLeft_ = counter[227] != 0;
            commandRight_ = counter[231] != 0;
            option_ = counter[226] != 0;
            shiftLeft_ = counter[225] != 0;
            shiftRight_ = counter[229] != 0;
            control_ = counter[224] != 0;
            fn_ = counter[4098] != 0;
            key_ = key;
        }
        
        bool operator<(const ComplexKey &ck) const {
            if(key_ < ck.key_){
                return true;
            }else if(key_ > ck.key_){
                return false;
            }else{
                if(commandLeft_ < ck.commandLeft_){
                    return true;
                }else if(commandLeft_ > ck.commandLeft_){
                    return false;
                }else{
                    if(commandRight_ < ck.commandRight_){
                        return true;
                    }else if(commandRight_ > ck.commandRight_){
                        return false;
                    }else{
                        if(option_ < ck.option_){
                            return true;
                        }else if(option_ > ck.option_){
                            return false;
                        }else{
                            if(shiftLeft_ < ck.shiftLeft_){
                                return true;
                            }else if(shiftLeft_ > ck.shiftLeft_){
                                return false;
                            }else{
                                if(shiftRight_ < ck.shiftRight_){
                                    return true;
                                }else if(shiftRight_ > ck.shiftRight_){
                                    return false;
                                }else{
                                    if(control_ < ck.control_){
                                        return true;
                                    }else if(control_ > ck.control_){
                                        return false;
                                    }else{
                                        if(fn_ < ck.fn_){
                                            return true;
                                        }else if(fn_ > ck.fn_){
                                            return false;
                                        }else{
                                            return false;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        bool operator==(const ComplexKey &ck) const {
            if(key_ != ck.key_)return false;
            if(commandLeft_ != ck.commandLeft_)return false;
            if(commandRight_ != ck.commandRight_)return false;
            if(option_ != ck.option_)return false;
            if(shiftLeft_ != ck.shiftLeft_)return false;
            if(shiftRight_ != ck.shiftRight_)return false;
            if(control_ != ck.control_)return false;
            if(fn_ != ck.fn_)return false;
            return true;
        }
        
        
        bool isHit(int* counter, krbn::key_code key){
            ComplexKey ck(counter, key);
            return *this==ck;
        }
        
        bool commandLeft_;
        bool commandRight_;
        bool option_;
        bool shiftLeft_;
        bool shiftRight_;
        bool control_;
        bool fn_;
        krbn::key_code key_;
    };
    
    
    int counter_[5000];
    std::map<ComplexKey, ComplexKey> ck2ck_;
    bool shikakari_ = false;
    ComplexKey shikakariSrc_;
    ComplexKey shikakariDst_;
    //=== takahasix ============================================
    //=== takahasix ============================================
    //=== takahasix ============================================
    
};
}
