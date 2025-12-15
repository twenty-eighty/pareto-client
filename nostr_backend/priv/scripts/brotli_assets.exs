static_root = Path.expand("priv/static")

# Only brotli-compress text-ish assets.
compressible_exts =
  MapSet.new([
    ".css",
    ".js",
    ".mjs",
    ".json",
    ".html",
    ".xml",
    ".txt",
    ".svg",
    ".map",
    ".md",
    ".webmanifest"
  ])

datetime_to_secs = fn dt -> :calendar.datetime_to_gregorian_seconds(dt) end

needs_regen? = fn src, dest ->
  case File.stat(dest) do
    {:ok, dest_stat} ->
      case File.stat(src) do
        {:ok, src_stat} -> datetime_to_secs.(src_stat.mtime) > datetime_to_secs.(dest_stat.mtime)
        _ -> false
      end

    _ ->
      true
  end
end

compress_file = fn src, dest ->
  case File.read(src) do
    {:ok, bin} ->
      case :brotli.encode(bin, %{quality: 11}) do
        {:ok, br_bin} ->
          File.write!(dest, br_bin)
          :ok

        error ->
          IO.warn("Brotli encode failed for #{src}: #{inspect(error)}")
          :error
      end

    {:error, reason} ->
      IO.warn("Failed to read #{src}: #{inspect(reason)}")
      :error
  end
end

IO.puts("Generating Brotli (.br) assets in #{static_root}")

static_root
|> Path.join("**/*")
|> Path.wildcard(match_dot: true)
|> Enum.reject(&File.dir?/1)
|> Enum.each(fn path ->
  ext = Path.extname(path)

  cond do
    ext == ".br" or ext == ".gz" ->
      :ok

    not MapSet.member?(compressible_exts, ext) ->
      :ok

    true ->
      br_path = path <> ".br"

      if needs_regen?.(path, br_path) do
        _ = compress_file.(path, br_path)
      end
  end
end)
