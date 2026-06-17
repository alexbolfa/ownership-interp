local M = {}

local namespace = vim.api.nvim_create_namespace("ownership-interp")
local group = vim.api.nvim_create_augroup("ownership-interp", { clear = true })

local defaults = {
  command = { "dune", "exec", "-j", "auto", "./bin/main.exe", "--", "--check" },
  root_markers = { "dune-project", "ownership_interp.opam", ".git" },
  debounce_ms = 120,
  virtual_text = true,
  signs = true,
  underline = true,
  update_in_insert = false,
}

local config = vim.deepcopy(defaults)
local timers = {}

local function find_root(path)
  local dir = vim.fs.dirname(path)
  local found = vim.fs.find(config.root_markers, { upward = true, path = dir })[1]
  return found and vim.fs.dirname(found) or vim.fn.getcwd()
end

local function parse_diagnostic(line)
  local file, lnum, col, message = line:match("^(.-):(%d+):(%d+):%s*(.*)$")
  if not file then
    return nil
  end

  return {
    lnum = math.max(tonumber(lnum) - 1, 0),
    col = math.max(tonumber(col) - 1, 0),
    end_lnum = math.max(tonumber(lnum) - 1, 0),
    end_col = math.max(tonumber(col), 1),
    severity = vim.diagnostic.severity.ERROR,
    source = "ownership-interp",
    message = message,
  }
end

local function collect_diagnostics(lines)
  local diagnostics = {}
  local fallback = {}

  for _, line in ipairs(lines) do
    local diagnostic = parse_diagnostic(line)
    if diagnostic then
      table.insert(diagnostics, diagnostic)
    elseif line ~= "" then
      table.insert(fallback, line)
    end
  end

  if #diagnostics == 0 and #fallback > 0 then
    table.insert(diagnostics, {
      lnum = 0,
      col = 0,
      end_lnum = 0,
      end_col = 1,
      severity = vim.diagnostic.severity.ERROR,
      source = "ownership-interp",
      message = table.concat(fallback, "\n"),
    })
  end

  return diagnostics
end

function M.check(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(bufnr) then
    return
  end

  local path = vim.api.nvim_buf_get_name(bufnr)
  if path == "" then
    return
  end

  vim.system(vim.list_extend(vim.deepcopy(config.command), { path }), {
    cwd = find_root(path),
    text = true,
  }, function(result)
    vim.schedule(function()
      if not vim.api.nvim_buf_is_valid(bufnr) then
        return
      end

      local output = vim.split((result.stderr or "") .. (result.stdout or ""), "\n", {
        plain = true,
        trimempty = true,
      })
      vim.diagnostic.set(namespace, bufnr, collect_diagnostics(output))
    end)
  end)
end

local function schedule(bufnr)
  if timers[bufnr] then
    timers[bufnr]:stop()
    timers[bufnr]:close()
  end

  timers[bufnr] = vim.defer_fn(function()
    timers[bufnr] = nil
    M.check(bufnr)
  end, config.debounce_ms)
end

function M.setup(opts)
  config = vim.tbl_deep_extend("force", vim.deepcopy(defaults), opts or {})

  vim.diagnostic.config({
    virtual_text = config.virtual_text,
    signs = config.signs,
    underline = config.underline,
    update_in_insert = config.update_in_insert,
  }, namespace)

  vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter" }, {
    group = group,
    pattern = "*.lang",
    callback = function(args)
      schedule(args.buf)
    end,
  })

  vim.api.nvim_create_user_command("OwnershipInterpCheck", function()
    M.check(0)
  end, {})
end

return M
