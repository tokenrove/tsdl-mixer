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

let open_audio =
  foreign "Mix_OpenAudio" (int @-> int @-> int @-> int @-> returning int)

type _chunk
let chunk_struct : _chunk structure typ = structure "Mix_Chunk"
let chunk : _chunk structure ptr typ = ptr chunk_struct
let chunk_opt : _chunk structure ptr option typ = ptr_opt chunk_struct

type _music
let music_struct : _music structure typ = structure "Mix_Music"
let music : _music structure ptr typ = ptr music_struct
let music_opt : _music structure ptr option typ = ptr_opt music_struct

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

let free_chunk =
  foreign "Mix_FreeChunk" (chunk @-> returning void)

let free_music =
  foreign "Mix_FreeMusic" (music @-> returning void)

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

end
