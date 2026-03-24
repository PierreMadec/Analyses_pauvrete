local prefixes = { 'fig', 'tip', 'tbl' }
local opened = {}
local str = pandoc.utils.stringify
local doc = ""
local file = ""
local file_id = ""

-- io.tmpname() does not work on windows and generate an error (not in workin directory)
-- we bypass that by creating a unique name any way
function file_exists(name)
   local f = io.open(name, "r")
   return f ~= nil and io.close(f)
end

function open_files()
  quarto.log.debug("====== opening files")
  for i, prefix in pairs(prefixes) do
    quarto.log.debug("======== prefix" .. i .. " " .. prefix)
    --tmp = pandoc.path.filename(os.tmpname())
    tmp = "_" .. file_id  .. "_" .. prefix .. "__.yml"
    opened[prefix] = io.open(tmp, "w")
  end
end

function close_files()
  if type(next(opened)) == "nil" then
    quarto.log.debug("===== no file to close")
    return nil
  end
  quarto.log.debug("====== closing files")
  for i, prefix in pairs(prefixes) do
    io.close(opened[prefix])
  end
end

function initialize(meta)
  dirsep = package .config :sub( 1, 1 )
  if quarto.project.directory then
    my_directory = quarto.project.directory .. "/.crossrefs"
  else
    my_directory = ".crossrefs"
  end
  if meta.title then
    doc = pandoc.utils.stringify(meta.title)
  else
    doc = ""
  end
  if quarto.project.directory then
    file = string.gsub(quarto.doc.input_file, quarto.project.directory .. dirsep, "")
    file = string.gsub(file, "\\", "/")
    file_id  = string.gsub(file, "/", "-")
  else
    file = "unknow"
  end
  pandoc.system.make_directory(my_directory, 1)
  pandoc.system.with_working_directory(my_directory, open_files)
  return meta
end

function get_match(s)
  local m
  local i = 0
  repeat
    i = i + 1
    m = string.match(s, prefixes[i].."[-]")
  until i == #prefixes or m
  if m then
    quarto.log.debug("matched "..m.." to "..prefixes[i])
    return prefixes[i]
  else
    return nil
  end
end

function add_listing(el)
  quarto.log.debug("------- add_listing")
  m = get_match(el.attr.identifier)
  if m then
     titre = str(el.content[1].content[2].content)
     call = "@" .. el.attr.identifier
     if file ~= "" then
       file_html = string.gsub(file, "qmd", "html")
       href =  "[" .. titre .. "](/" .. file_html .. "#" ..  el.attr.identifier .. ")"
     else
       href = call
     end
     newguy = { call = call,
                href =  href,
                doc = doc,
                file = file }

      io.output(opened[m])
      quarto.log.debug("====== writing " .. newguy["call"])
      h = "- "
      for key, val in pairs(newguy) do
        io.write(h .. key .. ': "' .. val .. '"\n')
        h = "  "
      end
  end
end

return {
  { Meta = initialize },
  { Div = add_listing },
  { Meta = close_files },
}
