function [ attribute ] = convert_to_labels( attribute )
for k=1:size(attribute, 1)
    value = attribute{k, 1};
    if value == 'y'
        attribute{k,1} = 1;
    elseif value == 'n'
        attribute{k,1} = -1;
    end
end
attribute = cell2mat(attribute);
end

