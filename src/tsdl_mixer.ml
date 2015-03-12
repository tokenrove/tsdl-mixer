open Ctypes
open Foreign
open Tsdl

module Mixer = struct

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

type _music
let music_struct : _music structure typ = structure "Mix_Music"
let music : _music structure ptr typ = ptr music_struct
let music_opt : _music structure ptr option typ = ptr_opt music_struct

let open_audio =
  foreign "Mix_OpenAudio" (int @-> int @-> int @-> int @-> returning int)

let allocate_channels n =
  foreign "Mix_AllocateChannels" (int @-> returning int)

(* extern DECLSPEC int SDLCALL Mix_QuerySpec(int *frequency,Uint16 *format,int *channels); *)

let load_wav_rw =
  foreign "Mix_LoadWAV_RW" (Sdl.rw_ops @-> int @-> returning chunk_opt)

let (>>=) o f =
  match o with | `Error e -> failwith (Printf.sprintf "Error %s" e)
               | `Ok a -> f a

let load_wav file =
  Sdl.rw_from_file file "rb" >>= fun rw ->
  load_wav_rw rw 1

let load_mus =
  foreign "Mix_LoadMUS" (string @-> returning music_opt)

let load_mus_rw =
  foreign "Mix_LoadMUS_RW" (Sdl.rw_ops @-> int @-> returning music_opt)

let load_mus_type_rw =
  foreign "Mix_LoadMUSType_RW" (Sdl.rw_ops @-> music_type @-> int @-> returning music_opt)

let quickload_wav =
  foreign "Mix_QuickLoad_WAV" (ptr uint8_t @-> returning chunk_opt)

let quickload_raw =
  foreign "Mix_QuickLoad_RAW" (ptr uint8_t @-> uint32_t @-> returning chunk_opt)

let free_chunk =
  foreign "Mix_FreeChunk" (chunk @-> returning void)

let free_music =
  foreign "Mix_FreeMusic" (music @-> returning void)

(*
 * extern DECLSPEC int SDLCALL Mix_GetNumChunkDecoders(void);
 * extern DECLSPEC const char * SDLCALL Mix_GetChunkDecoder(int index);
 * extern DECLSPEC int SDLCALL Mix_GetNumMusicDecoders(void);
 * extern DECLSPEC const char * SDLCALL Mix_GetMusicDecoder(int index);
 *)

let get_music_type =
  foreign "Mix_GetMusicType" (music_opt @-> returning music_type)

let play_channel_timed =
  foreign "Mix_PlayChannelTimed" (int @-> chunk @-> int @-> int @-> returning int)

let play_channel channel chunk loops =
  play_channel_timed channel chunk loops (-1)

let play_music =
  foreign "Mix_PlayMusic" (music @-> int @-> returning int)

let fade_out_music =
  foreign "Mix_FadeOutMusic" (int @-> returning int)

let halt_music =
  foreign "Mix_HaltMusic" (void @-> returning int)

let bool =
  let read = function 0 -> false | _ -> true in
  let write = function true -> 1 | false -> 0 in
  view ~read ~write:write int

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
