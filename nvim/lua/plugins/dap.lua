return {
    {
        "mfussenegger/nvim-dap",
	dependencies = {
		"rcarriga/nvim-dap-ui",
		"nvim-neotest/nvim-nio",
	},
	config = function()
      local dap = require "dap"
      local ui = require "dapui"

	--- golang
	--dap.adapters.delve = function(callback, config)
	--    if config.mode == 'remote' and config.request == 'attach' then
	--        callback({
	--            type = 'server',
	--            host = config.host or '127.0.0.1',
	--            port = config.port or '38697'
	--        })
	--    else
	--        callback({
	--            type = 'server',
	--            port = '${port}',
	--            executable = {
	--                command = 'dlv',
	--                args = { 'dap', '-l', '127.0.0.1:${port}', '--log', '--log-output=dap' },
	--                detached = vim.fn.has("win32") == 0,
	--            }
	--        })
	--    end
	--end
	dap.adapters.delve = {
    type = "server",
    host = "127.0.0.1",
    port = 38697,
  }
	
	
	-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
	dap.configurations.go = {
	  {
	    type = "delve",
	    name = "Debug",
	    request = "launch",
	    program = "${file}"
	  },
	  {
	    type = "delve",
	    name = "Debug test", -- configuration for debugging test files
	    request = "launch",
	    mode = "test",
	    program = "${file}"
	  },
	  -- works with go.mod packages and sub packages 
	  {
	    type = "delve",
	    name = "Debug test (go.mod)",
	    request = "launch",
	    mode = "test",
	    program = "./${relativeFileDirname}"
	  } 
	}
	
	--rust
dap.adapters.lldb = {
  type = 'server',
  port = "${port}",
  executable = {
  command = 'codelldb', -- adjust as needed, must be absolute path
  args = { "--port", "${port}" },
  }
}
dap.configurations.rust = {
  {
    name = 'Launch',
    type = 'lldb',
    request = 'launch',
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    cwd = vim.fn.getcwd(),
    stopOnEntry = false,
    args = {},

    -- 💀
    -- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
    --
    --    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
    --
    -- Otherwise you might get the following error:
    --
    --    Error on launch: Failed to attach to the target process
    --
    -- But you should be aware of the implications:
    -- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html
    -- runInTerminal = false,
  },
}


      require("dapui").setup()
      
	dap.listeners.before.attach.dapui_config = function()
        ui.open()
      end
      dap.listeners.before.launch.dapui_config = function()
        ui.open()
      end
      dap.listeners.before.event_terminated.dapui_config = function()
        ui.close()
      end
      dap.listeners.before.event_exited.dapui_config = function()
        ui.close()
      end

      end,
    },
}
