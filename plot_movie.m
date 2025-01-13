
cur_dir = pwd;
cd('Result_07_fiber_model');
files_fiber = dir('fiber*.dat');
files_fluid = dir('step*.dat');

files_fiber = {files_fiber(:).name};
files_fluid = {files_fluid(:).name};

writerObj = VideoWriter('contours','Uncompressed AVI');
writerObj.FrameRate = 5; open(writerObj);
figure('position',[50,100,1500,600])
for i=1:1:size(files_fiber,2)
    data_fluid = importdata(files_fluid{i},' ',3);
    data_fluid_header = data_fluid.textdata;
    data_fluid = data_fluid.data;
    zlen = 1.6; % length in z-dir
    data_fiber = importdata(files_fiber{i},' ',3);
    data_fiber_header = data_fiber.textdata;
    data_fiber = data_fiber.data;
    clf;
    subplot(1,3,1);
        contourf(reshape(data_fluid(:,1),[26,26]), ...
                 reshape(data_fluid(:,2),[26,26]), ...
                 reshape(data_fluid(:,4),[26,26]),100,'EdgeColor','none')
        colormap('jet'); colorbar; clim([-0.15,0.15]); shading('interp');
        hold on;
        plot(data_fiber(:,1),data_fiber(:,2),'-k','linewidth',2);
        plot(data_fiber(:,4),data_fiber(:,5),'--k','linewidth',1);
        xlabel('x'); ylabel('y'); axis equal; title(sprintf('Contour: u, frame=%d',i));
        set(gca,'fontsize',14,'FontWeight','bold');

    subplot(1,3,2);
        contourf(reshape(data_fluid(:,1),[26,26]), ...
                 reshape(data_fluid(:,2),[26,26]), ...
                 reshape(data_fluid(:,5),[26,26]),100,'EdgeColor','none')
        colormap('jet'); colorbar; clim([-0.15,0.15]); shading('interp');
        hold on;
        plot(data_fiber(:,1),data_fiber(:,2),'-k','linewidth',2);
        plot(data_fiber(:,4),data_fiber(:,5),'--k','linewidth',1);
        xlabel('x'); ylabel('y'); axis equal; title(sprintf('Contour: v, frame=%d',i));
        set(gca,'fontsize',14,'FontWeight','bold');

    subplot(1,3,3);
        contourf(reshape(data_fluid(:,1),[26,26]), ...
                 reshape(data_fluid(:,2),[26,26]), ...
                 reshape(data_fluid(:,7),[26,26])-101325,100,'EdgeColor','none')
        colormap('jet'); colorbar; clim([-0.1,0.1]); shading('interp');
        hold on;
        plot(data_fiber(:,1),data_fiber(:,2),'-k','linewidth',2);
        plot(data_fiber(:,4),data_fiber(:,5),'--k','linewidth',1);
        xlabel('x'); ylabel('y'); axis equal; title(sprintf('Contour: p-patm, frame=%d',i));
        set(gca,'fontsize',14,'FontWeight','bold');


    frame = getframe(gcf) ;   
    writeVideo(writerObj, frame);

end
close(writerObj);

cd(cur_dir);