#version 330 core

layout(location = 0) in vec2 position;
uniform vec2 instance_position;

uniform mat4 screen_transformation;

void main() {
    gl_Position = screen_transformation * vec4(position + instance_position, 0, 1);
}
