-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
    -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "habamax" } },
  spec = {
    -- add your plugins here
    {
      "ntk148v/habamax.nvim",
      lazy = false,
      priority = 1000,
      config = function()
        vim.cmd([[colorscheme habamax]])
      end,
    },
    {
       "mfussenegger/nvim-dap",
       config = function()
         local dap = require('dap')

         dap.adapters.r = function(cb, config)
           if config.request == 'attach' then
             local port = (config.connect or config).port
             local host = (config.connect or config).host or '127.0.0.1'
             cb({
               type = 'server',
               port = assert(port, '`connect.port` is required for a R `attach` configuration'),
               host = host,
               options = {
                 source_filetype = 'r',
               },
             })
           else
             -- work-in-progress, execute/launch mode
             error("R DAP only configured for attach-mode")
           end
         end

         dap.configurations.r = {
           {
             type = 'r',
             request = 'attach',
             name = 'Attach to R process',
             host = "localhost",
             port = 18721
           }
         }
       end,
    },
  },
  -- Configure any other settings here. See the documentation for more details.
  -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "habamax" } },
  -- automatically check for plugin updates
  checker = { enabled = true, notify = false },
})
