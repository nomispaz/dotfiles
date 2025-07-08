{
  "mfussenegger/nvim-dap",
  dependencies = {
    "rcarriga/nvim-dap-ui", -- UI for DAP
    "williamboman/mason.nvim", -- Optional: for managing debuggers
    "jay-babu/mason-nvim-dap.nvim", -- Optional: bridge mason & da
  },
  config = function()
    require("dapui").setup()
    require("mason").setup()
    require("mason-nvim-dap").setup({
      ensure_installed = { "codelldb", "delve" }, -- Rust and Go
      automatic_setup = true,
    })

    local dap, dapui = require("dap"), require("dapui")
    dap.listeners.after.event_initialized["dapui_config"] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close()
    end
    dap.listeners.before.event_exited["dapui_config"] = function()
      dapui.close()
    end
  end
}
