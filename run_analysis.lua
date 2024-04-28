function dump(o)
	if type(o) == "table" then
		local s = "{ "
		for k, v in pairs(o) do
			if type(k) ~= "number" then
				k = '"' .. k .. '"'
			end
			s = s .. "[" .. k .. "] = " .. dump(v) .. ","
		end
		return s .. "} "
	else
		return tostring(o)
	end
end

function print_files()
	-- Get all lines from the buffer
	local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
	local buffer_content = table.concat(lines, "\n")

	-- Write to a temporary file
	local tmp_filename = "input_for_rscript.txt"
	local file = io.open(tmp_filename, "w")
	file:write(buffer_content)
	file:close()

	-- Execute the R script with the temporary file as input
	local command = "cat " .. tmp_filename .. " | Rscript prepare_ast.r"
	local handle = io.popen(command)
	local output = handle:read("*a")
	handle:close()

	-- Optionally, remove the temporary file after use
	os.remove(tmp_filename)

	vim.cmd("tabnew")
	local new_buf = vim.api.nvim_get_current_buf()
	output = output:sub(1, -2)
	local lines = vim.split(output, "\\n")

	-- print(dump(lines))
	vim.api.nvim_buf_set_lines(new_buf, -1, -1, false, lines)
end

-- return {
-- 	run_r_script_with_buffer_input = print_files
-- }

-- luafile run_r_script.lua
-- lua require'run_r_script'.print_files()
