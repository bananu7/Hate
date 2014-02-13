#version 400 core

layout(location = 0) in vec2 position;
uniform vec2 instance_position;

void main() {
    gl_Position = vec4(position + instance_position, 0, 1);
}
