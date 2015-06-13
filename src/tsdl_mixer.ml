open Ctypes
open Foreign
open Tsdl

module Mixer = struct

let bool =
  let read = function 0 -> false | _ -> true in
  let write = function true -> 1 | false -> 0 in
  view ~read ~write:write int

module Init = struct
  type t = Unsigned.uint32
  let i = Unsigned.UInt32.of_int
  let ( + ) = Unsigned.UInt32.logor
  let test f m = Unsigned.UInt32.(compare (logand f m) zero <> 0)
  let eq f f' = Unsigned.UInt32.(compare f f' = 0)
  let empty = i 0
  let flac = i 1
  let mikmod = i 2
  let modplug = i 4
  let mp3 = i 8
  let ogg = i 16
  let fluidsynth = i 32
end

let init =
  foreign "Mix_Init" (uint32_t @-> returning uint32_t)

let quit =
  foreign "Mix_Quit" (void @-> returning void)

let mix_channels = 8
let default_frequency = 22050
let default_format = Sdl.Audio.s16_sys
let default_channels = 2
let max_volume = 128

type fading = NoFading | FadingOut | FadingIn
type music_type = None | Cmd | Wav | Mod | Mid | Ogg | Mp3 | Mp3_Mad | Flac | Modplug

let fading =
  let read = function 0 -> NoFading | 1 -> FadingOut | 2 -> FadingIn | _ -> failwith "Unexpected value" in
  let write = function NoFading -> 0 | FadingOut -> 1 | FadingIn -> 2 in
  view ~read ~write int

let music_type =
  let read = function 0 -> None | 1 -> Cmd | 2 -> Wav | 3 -> Mod | 4 -> Mid | 5 -> Ogg | 6 -> Mp3 | 7 -> Mp3_Mad  | 8 -> Flac | 9 -> Modplug | _ -> failwith "Unexpected value" in
  let write = function None -> 0 | Cmd -> 1 | Wav -> 2 | Mod -> 3 | Mid -> 4 | Ogg -> 5 | Mp3 -> 6 | Mp3_Mad -> 7 | Flac -> 8 | Modplug -> 9 in
  view ~read ~write int

type _chunk
let chunk_struct : _chunk structure typ = structure "Mix_Chunk"
let chunk : _chunk structure ptr typ = ptr chunk_struct
let chunk_opt : _chunk structure ptr option typ = ptr_opt chunk_struct
type chunk = _chunk structure ptr

type _music
let music_struct : _music structure typ = structure "Mix_Music"
let music : _music structure ptr typ = ptr music_struct
let music_opt : _music structure ptr option typ = ptr_opt music_struct
type music = _music structure ptr

let open_audio =
  foreign "Mix_OpenAudio" (int @-> int @-> int @-> int @-> returning int)

let allocate_channels n =
  foreign "Mix_AllocateChannels" (int @-> returning int)

let query_spec =
  foreign "Mix_QuerySpec" (ptr int @-> ptr uint16_t @-> ptr int @-> returning int)

let rw_ops =
  view ~read:Sdl.unsafe_rw_ops_of_ptr ~write:Sdl.unsafe_ptr_of_rw_ops nativeint

let load_wav_rw =
  foreign "Mix_LoadWAV_RW" (rw_ops @-> int @-> returning chunk_opt)

let (>>=) o f =
  match o with | `Error e -> failwith (Printf.sprintf "Error %s" e)
               | `Ok a -> f a

let load_wav file =
  Sdl.rw_from_file file "rb" >>= fun rw ->
  load_wav_rw rw 1

let load_mus =
  foreign "Mix_LoadMUS" (string @-> returning music_opt)

let load_mus_rw =
  foreign "Mix_LoadMUS_RW" (rw_ops @-> int @-> returning music_opt)

let load_mus_type_rw =
  foreign "Mix_LoadMUSType_RW" (rw_ops @-> music_type @-> int @-> returning music_opt)

let quickload_wav =
  foreign "Mix_QuickLoad_WAV" (ptr uint8_t @-> returning chunk_opt)

let quickload_raw =
  foreign "Mix_QuickLoad_RAW" (ptr uint8_t @-> uint32_t @-> returning chunk_opt)

let free_chunk =
  foreign "Mix_FreeChunk" (chunk @-> returning void)

let free_music =
  foreign "Mix_FreeMusic" (music @-> returning void)

let get_num_chunk_decoders =
  foreign "Mix_GetNumChunkDecoders" (void @-> returning int)
let get_chunk_decoder =
  foreign "Mix_GetChunkDecoder" (int @-> returning string)
let get_num_music_decoders =
  foreign "Mix_GetNumMusicDecoders" (void @-> returning int)
let get_music_decoder =
  foreign "Mix_GetMusicDecoder" (int @-> returning string)

let get_music_type =
  foreign "Mix_GetMusicType" (music_opt @-> returning music_type)

let mix_func = ptr void @-> ptr uint8_t @-> int @-> returning void
let set_post_mix =
  foreign "Mix_SetPostMix" (funptr mix_func @-> ptr void @-> returning void)
let hook_music =
  foreign "Mix_HookMusic" (funptr mix_func @-> ptr void @-> returning void)
let hook_music_finished =
  foreign "Mix_HookMusicFinished" (funptr (void @-> returning void) @-> returning void)
let get_music_hook_data =
  foreign "Mix_GetMusicHookData" (void @-> returning (ptr void))
let channel_finished =
  foreign "Mix_ChannelFinished" (funptr (int @-> returning void) @-> returning void)

let channel_post = -2

let effect_func_t = int @-> ptr void @-> int @-> ptr void @-> returning void
let effect_done_t = int @-> ptr void @-> returning void

let register_effect =
  foreign "Mix_RegisterEffect" (int @-> funptr effect_func_t @-> funptr effect_done_t @-> ptr void @-> returning int)
let unregister_effect =
  foreign "Mix_UnregisterEffect" (int @-> funptr effect_func_t @-> returning int)

let unregister_all_effects =
  foreign "Mix_UnregisterAllEffects" (int @-> returning int)

let effects_max_speed = "MIX_EFFECTSMAXSPEED"

let set_panning =
  foreign "Mix_SetPanning" (int @-> uint8_t @-> uint8_t @-> returning int)
let set_position =
  foreign "Mix_SetPosition" (int @-> int16_t @-> uint8_t @-> returning int)
let set_distance =
  foreign "Mix_SetDistance" (int @-> uint8_t @-> returning int)
let set_reverse_stereo =
  foreign "Mix_SetReverseStereo" (int @-> int @-> returning int)

let reserve_channels =
  foreign "Mix_ReserveChannels" (int @-> returning int)

let group_channel =
  foreign "Mix_GroupChannel" (int @-> int @-> returning bool)

let group_channels =
  foreign "Mix_GroupChannels" (int @-> int @-> int @-> returning bool)

let group_available =
  foreign "Mix_GroupAvailable" (int @-> returning int)

let group_count =
  foreign "Mix_GroupCount" (int @-> returning int)

let group_oldest =
  foreign "Mix_GroupOldest" (int @-> returning int)

let group_newer =
  foreign "Mix_GroupNewer" (int @-> returning int)

let play_channel_timed =
  foreign "Mix_PlayChannelTimed" (int @-> chunk @-> int @-> int @-> returning int)

let play_channel channel chunk loops =
  play_channel_timed channel chunk loops (-1)

let play_music =
  foreign "Mix_PlayMusic" (music @-> int @-> returning int)

let fade_in_music =
  foreign "Mix_FadeInMusic" (music @-> int @-> int @-> returning int)
let fade_in_music_pos =
  foreign "Mix_FadeInMusicPos" (music @-> int @-> int @-> double @-> returning int)
let fade_in_channel_timed =
  foreign "Mix_FadeInChannelTimed" (int @-> chunk @-> int @-> int @-> int @-> returning int)
let fade_in_channel channel chunk loops ms =
  fade_in_channel_timed channel chunk loops ms (-1)

let volume =
  foreign "Mix_Volume" (int @-> int @-> returning int)
let volume_chunk =
  foreign "Mix_VolumeChunk" (chunk @-> int @-> returning int)
let volume_music =
  foreign "Mix_VolumeMusic" (int @-> returning int)

let halt_channel =
  foreign "Mix_HaltChannel" (int @-> returning int)
let halt_group =
  foreign "Mix_HaltGroup" (int @-> returning int)
let halt_music =
  foreign "Mix_HaltMusic" (void @-> returning int)

let expire_channel =
  foreign "Mix_ExpireChannel" (int @-> int @-> returning int)

let fade_out_channel =
  foreign "Mix_FadeOutChannel" (int @-> int @-> returning int)
let fade_out_group =
  foreign "Mix_FadeOutGroup" (int @-> int @-> returning int)
let fade_out_music =
  foreign "Mix_FadeOutMusic" (int @-> returning int)

let set_music_cmd =
  foreign "Mix_SetMusicCMD" (string @-> returning int)

let set_synchro_value =
  foreign "Mix_SetSynchroValue" (int @-> returning int)
let get_synchro_value =
  foreign "Mix_GetSynchroValue" (void @-> returning int)

 let set_sound_fonts =
  foreign "Mix_SetSoundFonts" (string @-> returning int)
let get_sound_fonts =
  foreign "Mix_GetSoundFonts" (void @-> returning string)
let each_sound_font =
  foreign "Mix_EachSoundFont" (funptr (string @-> ptr void @-> returning int) @-> ptr void @-> returning int)

let fading_music =
  foreign "Mix_FadingMusic" (void @-> returning fading)

let fading_channel =
  foreign "Mix_FadingChannel" (int @-> returning fading)

let pause = foreign "Mix_Pause" (int @-> returning void)
let resume = foreign "Mix_Resume" (int @-> returning void)
let paused = foreign "Mix_Paused" (int @-> returning bool)

let pause_music =
  foreign "Mix_PauseMusic" (void @-> returning void)
let resume_music =
  foreign "Mix_ResumeMusic" (void @-> returning void)
let rewind_music =
  foreign "Mix_RewindMusic" (void @-> returning void)
let paused_music = foreign "Mix_PausedMusic" (void @-> returning bool)

let set_music_position =
  foreign "Mix_SetMusicPosition" (double @-> returning int)

let playing =
  foreign "Mix_Playing" (int @-> returning bool)

let playing = function
  | Some channel -> playing channel
  | None -> playing (-1)

let playing_music =
  foreign "Mix_PlayingMusic" (void @-> returning bool)

let get_chunk =
  foreign "Mix_GetChunk" (int @-> returning chunk_opt)

let close_audio =
  foreign "Mix_CloseAudio" (void @-> returning void)

end
