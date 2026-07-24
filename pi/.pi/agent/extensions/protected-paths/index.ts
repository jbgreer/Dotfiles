// ~/.pi/agent/extensions/protected-paths/index.protected-paths
const SENSITIVE = [/\.env$/, /id_rsa/, /\.pem$/, /\/\.ssh\//, /\/\.aws\//];

export default (pi) => {
  pi.on("tool_call", async (call, ctx) => {
    const target = call.input_path ?? call.input.command ?? "";
    if (!SENSITIVE.some((r) => r.test(target))) return;
    const ok = await ctx.ui.confirm(`Allow ${call_name} on ${target}?`);
    if (!ok) return { block: true, reason: "Blocked by user" };
  });
}
