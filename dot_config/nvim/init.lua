--[[

=====================================================================
==================== READ THIS BEFORE CONTINUING ====================
=====================================================================

Kickstart.nvim is *not* a distribution.

Kickstart.nvim is a template for your own configuration.
  The goal is that you can read every line of code, top-to-bottom, understand
  what your configuration is doing, and modify it to suit your needs.

  Once you've done that, you should start exploring, configuring and tinkering to
  explore Neovim!

  If you don't know anything about Lua, I recommend taking some time to read through
  a guide. One possible example:
  - https://learnxinyminutes.com/docs/lua/


  And then you can explore or search through `:help lua-guide`
  - https://neovim.io/doc/user/lua-guide.html


Kickstart Guide:

I have left several `:help X` comments throughout the init.lua
You should run that command and read that help section for more information.

In addition, I have some `NOTE:` items throughout the file.
These are for you, the reader to help understand what is happening. Feel free to delete
them once you know what you're doing, but they should serve as a guide for when you
are first encountering a few different constructs in your nvim config.

I hope you enjoy your Neovim journey,
- TJ

P.S. You can delete this when you're done too. It's your config now :)
--]]
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install package manager
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- NOTE: Here is where you install your plugins.
--  You can configure plugins using the `config` key.
--
--  You can also configure plugins after the setup call,
--    as they will be available in your neovim runtime.
require('lazy').setup({
  -- NOTE: First, some plugins that don't require any configuration

  -- Git related plugins
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  -- NOTE: This is where your plugins related to LSP can be installed.
  --  The configuration is done below. Search for lspconfig to find it below.
  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',

      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim', tag = 'legacy', opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
  },

  {
    -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',

      -- Adds signature help
      'hrsh7th/cmp-nvim-lsp-signature-help',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',
    },
  },

  -- Useful plugin to show you pending keybinds.
  { 'folke/which-key.nvim',  opts = {} },
  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        map({ 'n' }, '<leader>hp', require('gitsigns').preview_hunk, { desc = 'Preview git hunk' })
        map({ 'n' }, '<leader>hs', gs.stage_hunk, { desc = '[s]tage hunk' })
        map({ 'n' }, '<leader>hr', gs.reset_hunk, { desc = '[r]eset hunk' })
        map({ 'v' }, '<leader>hs', function() gs.stage_hunk { vim.fn.line('.'), vim.fn.line('v') } end,
          { desc = '[s]tage hunk' })
        map({ 'v' }, '<leader>hr', function() gs.reset_hunk { vim.fn.line('.'), vim.fn.line('v') } end,
          { desc = '[r]eset hunk' })
        map({ 'n' }, '<leader>hS', gs.stage_buffer, { desc = '[S]tage buffer' })
        map({ 'n' }, '<leader>hu', gs.undo_stage_hunk, { desc = '[u]ndo stage hunk' })
        map({ 'n' }, '<leader>hR', gs.reset_buffer, { desc = '[R]eset buffer' })
        map({ 'n' }, '<leader>hp', gs.preview_hunk, { desc = '[p]review git hunk' })
        map({ 'n' }, '<leader>hb', function() gs.blame_line { full = true } end, { desc = '[b]lame line' })
        map({ 'n' }, '<leader>tb', gs.toggle_current_line_blame, { desc = 'toggle current line [b]lame' })
        map({ 'n' }, '<leader>hd', gs.diffthis, { desc = '[d]iff this' })
        map({ 'n' }, '<leader>hD', function() gs.diffthis('~') end, { desc = '[D]iff this with ~ (?)' })
        map({ 'n' }, '<leader>td', gs.toggle_deleted, { desc = 'toggle [d]eleted' })

        -- Text object
        vim.keymap.set({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>')

        -- don't override the built-in and fugitive keymaps
        vim.keymap.set({ 'n', 'v' }, ']c', function()
          if vim.wo.diff then
            return ']c'
          end
          vim.schedule(function()
            gs.next_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, buffer = bufnr, desc = 'Jump to next hunk' })
        vim.keymap.set({ 'n', 'v' }, '[c', function()
          if vim.wo.diff then
            return '[c'
          end
          vim.schedule(function()
            gs.prev_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, buffer = bufnr, desc = 'Jump to previous hunk' })
      end,
    },
  },

  -- {
  --   -- Theme inspired by Atom
  --   'navarasu/onedark.nvim',
  --   priority = 1000,
  --   -- config = function()
  --   --   vim.cmd.colorscheme 'onedark'
  --   -- end,
  -- },

  {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = false,
        theme = 'forestbones',
        component_separators = '|',
        section_separators = '',
      },
      -- sections = {lualine_c = {require('auto-session.lib').current_session_name}},
    },
  },

  -- {
  --   -- Add indentation guides even on blank lines
  --   'lukas-reineke/indent-blankline.nvim',
  --   -- Enable `lukas-reineke/indent-blankline.nvim`
  --   -- See `:help ibl`
  --   main = 'ibl',
  --   opts = {},
  -- },

  -- "gc" to comment visual regions/lines
  { 'numToStr/Comment.nvim', opts = {} },

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- Fuzzy Finder Algorithm which requires local dependencies to be built.
      -- Only load if `make` is available. Make sure you have the system
      -- requirements installed.
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        -- NOTE: If you are having trouble with this installation,
        --       refer to the README for telescope-fzf-native for more instructions.
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
      {
        'nvim-telescope/telescope-ui-select.nvim',
      },
    },
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
      'nvim-treesitter/nvim-treesitter-refactor',
      'nvim-treesitter/nvim-treesitter-context',
    },
    build = ':TSUpdate',
  },

  -- NOTE: Next Step on Your Neovim Journey: Add/Configure additional "plugins" for kickstart
  --       These are some example plugins that I've included in the kickstart repository.
  --       Uncomment any of the lines below to enable them.
  -- require 'kickstart.plugins.autoformat',
  -- require 'kickstart.plugins.debug',

  -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
  --    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
  --    up-to-date with whatever is in the kickstart repo.
  --    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
  --
  --    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
  -- { import = 'custom.plugins' },

  {
    'mcchrish/zenbones.nvim',
    priority = 1000,
    dependencies = {
      'rktjmp/lush.nvim',
    },
    config = function()
      vim.o.background = 'dark'
      -- vim.g.zenburned = { darkness = 'stark' }
      -- vim.cmd.colorscheme 'zenburned'
      vim.g.forestbones = { darkness = 'stark' }
      vim.cmd.colorscheme 'forestbones'
    end,
  },

  {
    'sainnhe/everforest',
    -- priority = 1000,
    config = function()
      -- vim.o.background = 'dark'
      -- vim.g.everforest_background = 'hard'
      -- vim.cmd.colorscheme 'everforest'
    end
  },

  -- {
  --   'mg979/vim-visual-multi',
  --   branch = 'master',
  --   config = function()
  --     vim.g.VM_theme = 'olive'
  --   end
  -- },

  {
    'tpope/vim-surround',
    branch = 'master',
  },

  {
    'windwp/nvim-autopairs',
  },

  {
    'fatih/vim-go'
  },

  {
    'rcarriga/nvim-dap-ui',
    dependencies = { 'mfussenegger/nvim-dap', 'leoluz/nvim-dap-go' },
    config = function()
      vim.keymap.set('n', '<leader>dt', function() require('dap-go').debug_test() end,
        { desc = '[d]ebug nearest [t]est' })
      vim.keymap.set('n', '<leader>du', function() require('dapui').toggle() end, { desc = 'toggle the dap [u]i' })
      vim.keymap.set('n', '<Leader>db', function() require('dap').toggle_breakpoint() end,
        { desc = 'Toggle [b]reakpoint' })
      vim.keymap.set('n', '<Leader>dB', function() require('dap').set_breakpoint() end, { desc = 'Set [B]reakpoint' })
      vim.keymap.set('n', '<Leader>dl',
        function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end,
        { desc = '[l]og point message' })
      vim.keymap.set('n', '<F5>', function() require('dap').continue() end, { desc = 'Start debugging' })
      vim.keymap.set('n', '<F10>', function() require('dap').step_over() end, { desc = 'Step over' })
      vim.keymap.set('n', '<F11>', function() require('dap').step_into() end, { desc = 'Step into' })
      vim.keymap.set('n', '<F12>', function() require('dap').step_out() end, { desc = 'Step out' })
    end
  },

  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      -- start with lines turned off
      vim.diagnostic.config({ virtual_lines = false })
      require("lsp_lines").setup()
      -- but allow us to toggle between the two:
      vim.keymap.set("", "<Leader>tl",
        function()
          require("lsp_lines").toggle()
          local new_value = not vim.diagnostic.config().virtual_text
          vim.diagnostic.config({ virtual_text = new_value })
        end,
        { desc = "toggle [l]sp_lines" })
    end,
  },

  {
    'rmagatti/auto-session',
    config = function()
      require("auto-session").setup {
        log_level = "error",
        auto_session_suppress_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
        auto_session_enable_last_session = false,
        cwd_change_handling = {
          restore_upcoming_session = true,   -- already the default, no need to specify like this, only here as an example
          pre_cwd_changed_hook = nil,        -- already the default, no need to specify like this, only here as an example
          post_cwd_changed_hook = function() -- example refreshing the lualine status line _after_ the cwd changes
            require("lualine").refresh()     -- refresh lualine so the new session name is displayed in the status bar
          end,
        },
      }
      vim.keymap.set('n', "<C-s>", require("auto-session.session-lens").search_session,
        { noremap = true, desc = '[C-s]earch sessions' })
    end
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      "3rd/image.nvim",              -- Optional image support in preview window: See `# Preview Mode` for more information
    },
    config = function()
      require('neo-tree').setup {
        vim.keymap.set('n', "¡", '<Cmd>Neotree toggle reveal_force_cwd<cr>',
          { desc = 'Neotree toggle reveal current file' })
      }
    end
  },

  {
    'ruanyl/vim-gh-line',
    config = function()
      -- vim.keymap.set('n', '<leader>hl', require('vim-gh-line').gh_open_command)
    end
  },

  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
  },
  {
    'glacambre/firenvim',

    -- Lazy load firenvim
    -- Explanation: https://github.com/folke/lazy.nvim/discussions/463#discussioncomment-4819297
    lazy = not vim.g.started_by_firenvim,
    build = function()
      vim.fn["firenvim#install"](0)
    end
  },

  {
    'justinmk/vim-sneak',
    dependencies = { 'tpope/vim-repeat' },
    config = function()
      vim.keymap.set('n', 'f', '<Plug>Sneak_f', {})
      vim.keymap.set('n', 'F', '<Plug>Sneak_F', {})
      vim.keymap.set('n', 't', '<Plug>Sneak_t', {})
      vim.keymap.set('n', 'T', '<Plug>Sneak_T', {})
    end
  },

  {
    'gbprod/yanky.nvim',
    config = function()
      vim.keymap.set('n', '<leader>p', require('telescope').extensions.yank_history.yank_history,
        { desc = '[p]aste from yank history' })
      -- and if you want to paste from yanky and then cycle through the history:
      vim.keymap.set('n', 'p', '<Plug>(YankyPutAfter)')
      vim.keymap.set('n', 'P', '<Plug>(YankyPutBefore)')
      vim.keymap.set('n', '<c-n>', '<Plug>(YankyCycleForward)')
      vim.keymap.set('n', '<c-p>', '<Plug>(YankyCycleBackward)')
    end
  },

  {
    'neomake/neomake',
  },

  {
    'roxma/vim-tmux-clipboard',
  },

  {
    'tmux-plugins/vim-tmux-focus-events',
  },

  {
    'ziglang/zig.vim',
  },

  {
    "https://git.sr.ht/~nedia/auto-save.nvim",
    config = function()
      require('auto-save').setup({
        events = { 'FocusLost', 'BufLeave' },
      })
    end,
  },

  -- {
  --   'pocco81/auto-save.nvim',
  --   config = function()
  --     require('auto-save').setup {
  --       trigger_events = {"BufLeave", "FocusLost"},
  --       execution_message = {
  --         message = function() return ("AHHHH!!") end
  --       },
  --     }
  --   end
  -- },
}, {})

-- load my personal snippets
require("luasnip.loaders.from_snipmate").lazy_load({ paths = "~/.config/nvim/snippets" })

-- configure yany picker (like the emacs kill-ring)
require('yanky').setup({
  -- see https://github.com/gbprod/yanky.nvim
})

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
-- vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.number = true

-- minimum numbe of lines while scrolling
vim.o.scrolloff = 10

-- how much does c-d and c-u jump by
vim.o.scroll = 20

-- tabs should be 4
vim.o.shiftwidth = 4

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- use osc53 clipboard
require('clipboard')

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- set default tabstop, this fixes telescope's tendancy to put a tabstop of 8 in go previews
vim.o.tabstop = 4

-- split below and to the right
vim.o.splitbelow = true
vim.o.splitright = true

-- Code folding using treesitter
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldenable = false

-- [[ Basic Keymaps ]]

-- Fix neovim "fixing" default vim Y => yank whole line
vim.keymap.set('n', 'Y', 'Y')

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })


-- Primeagen's move highlighted text up and down:
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- Helix has some greate interactive mode keybindings:
vim.keymap.set({ 'i' }, '<C-d>', "<C-o>x")
vim.keymap.set({ 'i' }, '<M-d>', "<C-o>dw")
-- conflicts with signature help from lsp
-- vim.keymap.set({'i'}, '<C-k>', "<C-o>d$<right>")

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ reload files! ]]
-- Triger `autoread` when files changes on disk
-- https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
-- https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
vim.api.nvim_create_autocmd({ 'FocusGained', 'BufEnter', 'CursorHold', 'CursorHoldI' }, {
  pattern = '*',
  command = "if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif",
})

-- Notification after file change
-- https://vi.stackexchange.com/questions/13091/autocmd-event-for-autoread
vim.api.nvim_create_autocmd({ 'FileChangedShellPost' }, {
  pattern = '*',
  command = "echohl WarningMsg | echo 'File changed on disk. Buffer reloaded.' | echohl None",
})

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  extensions = {
    ["ui-select"] = { require("telescope.themes").get_dropdown({}) },
  },
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
      n = {
        ['d'] = "delete_buffer",
      }
    },
    cache_picker = { num_pickers = 10 },
    dynamic_preview_title = true,
    -- layout_strategy = "vertical",
    -- layout_config = {vertical = {width = 0.9, height = 0.9, preview_height = 0.6, preview_cutoff = 0}},
    layout_strategy = "horizontal",
    layout_config = { horizontal = { width = 0.9, height = 0.9, preview_cutoff = 0 } },
    path_display = { "smart", shorten = { len = 2 } }, -- only smart seems to work
    wrap_results = false,
    -- give more room for the filename:
  },
  pickers = {
    colorscheme = {
      enable_preview = true
    },
    buffers = {
      ignore_current_buffer = true,
      sort_mru = true,
    },
    current_buffer_tags = { fname_width = 40, },
    jumplist = { fname_width = 40, },
    loclist = { fname_width = 40, },
    lsp_definitions = { fname_width = 40, },
    lsp_document_symbols = { fname_width = 40, },
    lsp_dynamic_workspace_symbols = { fname_width = 40, },
    lsp_implementations = { fname_width = 40, },
    lsp_incoming_calls = { fname_width = 40, },
    lsp_outgoing_calls = { fname_width = 40, },
    lsp_references = { fname_width = 40, },
    lsp_type_definitions = { fname_width = 40, },
    lsp_workspace_symbols = { fname_width = 40, },
    quickfix = { fname_width = 40, },
    tags = { fname_width = 40, },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')
pcall(require('telescope').load_extension, 'yank_history')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>b', require('telescope.builtin').buffers, { desc = 'Find existing [b]uffers' })
vim.keymap.set('n', '<leader>/', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = '[/] Fuzzily search in current buffer' })

vim.keymap.set('n', '<leader>gf', require('telescope.builtin').git_files, { desc = 'Search [G]it [F]iles' })
vim.keymap.set('n', '<leader>sf',
  "<cmd>lua require('telescope.builtin').find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>",
  { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').resume, { desc = '[S]earch [R]esume' })
vim.keymap.set('n', '<leader>sp', require('telescope.builtin').pickers, { desc = '[S]earch [P]ickers' })

-- Some custom keybindings for convenience
vim.keymap.set('n', '<leader>ts', '<Cmd>set hlsearch!<cr>', { desc = 'toggle highlight [s]earch' })


-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'tsx', 'javascript', 'typescript', 'vimdoc', 'vim',
      'bash', 'zig', 'odin' },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    auto_install = false,

    highlight = { enable = true },
    indent = { enable = true },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = '<c-space>',
        node_incremental = '<c-space>',
        scope_incremental = '<c-s>',
        node_decremental = '<M-space>',
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ['aa'] = '@parameter.outer',
          ['ia'] = '@parameter.inner',
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
          ['<M-e>'] = '@function.name',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
          ['<M-a>'] = '@function.name',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ['<leader>a'] = '@parameter.inner',
        },
        swap_previous = {
          ['<leader>A'] = '@parameter.inner',
        },
      },
    },
    refactor = {
      highlight_definitions = {
        enable = true,
        -- Set to false if you have an `updatetime` of ~100.
        clear_on_cursor_move = true,
      },
    },
  }

  local ts_repeat_move = require("nvim-treesitter.textobjects.repeatable_move")

  -- vim way: ; goes to the direction you were moving.
  -- vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
  -- vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)

  -- Optionally, make builtin f, F, t, T also repeatable with ; and ,
  -- vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f)
  -- vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F)
  -- vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t)
  -- vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T)
end, 0)

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- Convenience keymaps similiar to IDEA (TODO: reorganize these)
vim.keymap.set('v', '<Leader>cd', "y'>p", { desc = '[d]uplicate lines or selection' })
vim.keymap.set('n', '<Leader>cd', "Vyp", { desc = '[d]uplicate lines or selection' })

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  -- This is so hacky, but I don't know how to wait for the async lsp command to return...
  local goto_def_or_ref = function()
    local row1, _ = unpack(vim.api.nvim_win_get_cursor(0))
    local res = require('telescope.builtin').lsp_definitions()
    vim.wait(1000, function() -- run immediately, then every 1000ms after until function() returns true.
      vim.cmd [[:sleep 100m]] -- sleep for 100 ms, then exit, so the 1000 in vim.wait never fires.
      local row2, _ = unpack(vim.api.nvim_win_get_cursor(0))
      print(row1, row2, res)
      if row1 == row2 then
        require('telescope.builtin').lsp_references()
      end
      return true
    end)
  end

  nmap('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')
  nmap('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

  nmap('gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')
  -- Alternative:
  nmap('<M-.>', goto_def_or_ref, '[G]oto [D]efinition')
  nmap('gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')
  nmap('gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')
  nmap('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')
  nmap('<leader>cs', require('telescope.builtin').lsp_document_symbols, 'document [S]ymbols')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  nmap('<leader>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
  nmap('<leader>f', vim.lsp.buf.format, '[f]ormat buffer')
end

-- document existing key chains
require('which-key').register {
  ['<leader>c'] = { name = '[C]ode', _ = 'which_key_ignore' },
  ['<leader>d'] = { name = '[D]ebug', _ = 'which_key_ignore' },
  ['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },
  ['<leader>h'] = { name = 'More git', _ = 'which_key_ignore' },
  ['<leader>t'] = { name = '[T]oggle things', _ = 'which_key_ignore' },
  ['<leader>r'] = { name = '[R]ename', _ = 'which_key_ignore' },
  ['<leader>s'] = { name = '[S]earch', _ = 'which_key_ignore' },
  ['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
}

-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup()

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
--
--  If you want to override the default filetypes that your language server will attach to you can
--  define the property 'filetypes' to the map in question.
local servers = {
  clangd = {},
  gopls = {},
  -- pyright = {},
  rust_analyzer = {},
  tsserver = {},
  html = { filetypes = { 'html', 'twig', 'hbs' } },
  zls = {},
  -- ols = {
  --   command = "/Users/chris/dev/odin/ols",
  --   filetypes = { "odin" },
  --   rootPatterns = { "ols.json" },
  -- },

  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}

if vim.loop.os_uname().sysname == 'Linux' then
  servers = {
    clangd = {},
    -- gopls = {},
    -- pyright = {},
    rust_analyzer = {},
    -- tsserver = {},
    -- html = { filetypes = { 'html', 'twig', 'hbs'} },

    lua_ls = {
      Lua = {
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
      },
    },
  }
end
-- Setup neovim lua configuration
require('neodev').setup({
  library = { plugins = { "nvim-dap-ui" }, types = true },
})

-- Setup dap-go
require('dapui').setup()
require('dap-go').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
    }
  end,
}

-- Because the zls setup needs cmd at the root, not in the settings object like above?
-- require'lspconfig'.zls.setup{
--   cmd = { '/Users/chris/git/zls/zig-out/bin/zls' }
-- }

-- same as zig? ... doesn't seem to work...
-- require'lspconfig'.ols.setup{
--   cmd = { "/Users/chris/bin/ols" },
--   filetypes = { "odin" },
--   root_dir = require'lspconfig'.util.root_pattern("ols.json", ".git"),
--   single_file_support = true,
-- }

-- [[ Configure nvim-cmp ]]
-- See `:help cmp`
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'nvim_lsp_signature_help' },
  },
}

--
-- [[ Configure nvim-autopairs ]]
require("nvim-autopairs").setup({
  check_ts = true,
  map_c_h = true,
  ts_config = {
    lua = { 'string' }, -- it will not add a pair on that treesitter node
    javascript = { 'template_string' },
    -- java = false,-- don't check treesitter on java
  },
  enable_check_bracket_line = false,
  ignored_next_char = "[%w%.]", -- will ignore alphanumeric and `.` symbol,
  fast_wrap = {
    map = "<M-e>",              -- slurp!
    chars = { "{", "[", "(", '"', "'" },
    pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
    offset = 0, -- Offset from pattern match
    end_key = "$",
    keys = "qwertyuiopzxcvbnmasdfghjkl",
    check_comma = true,
    highlight = "PmenuSel",
    highlight_grey = "LineNr",
  },
})

local cmp_autopairs = require "nvim-autopairs.completion.cmp"
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })

-------------------------
-- Load up my very small version of unimpaired
-- for ]<space> and [<space> mappings:
vim.cmd('source ~/.config/nvim/myunimpaired.vim')

--
-- Compiler commands
--
-- local augroup = vim.api.nvim_create_augroup("chris", { clear = true })
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "typescript,typescriptreact",
--   group = augroup,
--   command = "compiler tsc | setlocal makeprg=npm\\ run\\ lint\\ --\\ --format\\ unix",
-- })

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
