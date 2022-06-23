local telescope = require('telescope')
local action_layout = require("telescope.actions.layout")
telescope.setup {
	defaults = {
		layout_config = {
			vertical = { width = 0.5 }
		},
		mappings = {
			i = {
				["C-h"] = "which_key"
	 		},
			n = {
				["."] = action_layout.toggle_preview
			}
		},
	},
}

local utils = require('rom_utils')
utils.map('n', '<leader>ff', '<cmd>Telescope find_files<CR>')
utils.map('n', '<leader>fg', '<cmd>Telescope live_grep<cr>')
utils.map('n', '<leader>fb', '<cmd>Telescope buffers<cr>')
utils.map('n', '<leader>fh', '<cmd>Telescope help_tags<cr>')
utils.map('n', '<leader>f.', '<cmd> lua require(\'telescope.builtin\').find_files( { cwd = vim.fn.expand(\'%:p:h\') })<cr>')

