clear,clc
%% Writed By Dongdong Kong, 2016-10-19
%%% 把2474站点，全部整理成常用的时间序列格式
file = 'surface_met2474_daily.nc';
%%% try precipitation as example
info = ncinfo(file);
ncdisp(file)
vars = {info.Variables.Name};
nvar = length(vars);
[std, year, month, day] = info.Dimensions.Length;
%% rewrite nc files
datatype = 'single';
fillvalue = -999;
attrs = {info.Variables.Attributes};
file_new = 'surface_met2474_daily_kong2.nc';
if exist(file_new,'file'), delete(file_new); end

for i = 1: nvar - 3
    nccreate(file_new, vars{i},'Dimensions',{'day', day, 'month', month, 'year', year, 'std', std}, ...
        'Datatype', datatype, 'FillValue', -999)
end
nccreate(file_new,'lat', 'Dimensions',{'std' std}, 'Datatype', 'single')
nccreate(file_new,'lon','Dimensions',{'std' std}, 'Datatype', 'single')
nccreate(file_new,'alt','Dimensions',{'std' std}, 'Datatype', 'single')
%%% write attributes -------------
ncwriteatt(file_new,'/','period', '1951-2014');
ncwriteatt(file_new,'/','source', 'China surface meteorological observatins at 2474 stations');
ncwriteatt(file_new,'/','creation_time',datestr(now));
ncwriteatt(file_new,'/','author','Dongdong Kong, kongdd@mail2.sysu.edu.cn, Sun Yat-sen university');

ncwriteatt(file_new,'lat','long_name','latitude');
ncwriteatt(file_new,'lat','units','degrees_north');
ncwriteatt(file_new,'lat','standard_name','latitude');
ncwriteatt(file_new,'lon','long_name','longitude');
ncwriteatt(file_new,'lon','units','degrees_east');
ncwriteatt(file_new,'lon','standard_name','longitude');
ncwriteatt(file_new,'alt','long_name','altitude');
ncwriteatt(file_new,'alt','units','m');
ncwriteatt(file_new,'alt','standard_name','altitude');

for i = 1: nvar - 3
    ncwriteatt(file_new, vars{i}, 'long_name', attrs{i}(1).Value);
    ncwriteatt(file_new, vars{i}, 'units', attrs{i}(2).Value);
    ncwriteatt(file_new, vars{i}, 'FillValue', fillvalue);
end

tic
%%% read data from origin nc file
fprintf('Saving nc file ...\n')
for i = 1: nvar
    fprintf('[%d] %s\n', i, vars{i})
    data = ncread(file, vars{i});
    if i <= nvar - 3
        ncwrite(file_new, vars{i}, permute(data, [4, 3, 2, 1]))
    else
        ncwrite(file_new, vars{i}, data)
    end
end
toc
%%% write data into nc file
%%% ====================== SAVE NC FILES FINISHED =========================
finfo = dir(file_new);
finfo.bytes/1024^3

tic, x = ncread(file_new, 'T2M'); toc
tic, y = ncread(file, 'T2M'); toc