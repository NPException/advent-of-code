/*
 * Code found here: https://memorynotfound.com/generate-gif-image-java-delay-infinite-loop-example/
 * Adapted for easier use with clojure
 */
package de.npe.imageutils;

import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.ImageOutputStream;

public final class GifWriter implements AutoCloseable {

	private final ImageWriter writer;
	private final ImageWriteParam params;
	private final int delay;
	private final boolean loop;

	private IIOMetadata metadata;

	public GifWriter(ImageOutputStream out, int delayms, boolean loop) {
		delay = delayms / 10;
		this.loop = loop;
		writer = ImageIO.getImageWritersBySuffix("gif").next();
		writer.setOutput(out);
		params = writer.getDefaultWriteParam();
	}

	private IIOMetadata initMetaData(BufferedImage firstImage) throws IOException {
		ImageTypeSpecifier imageTypeSpecifier = ImageTypeSpecifier.createFromBufferedImageType(firstImage.getType());
		IIOMetadata metadata = writer.getDefaultImageMetadata(imageTypeSpecifier, params);

		String metaFormatName = metadata.getNativeMetadataFormatName();
		IIOMetadataNode root = (IIOMetadataNode) metadata.getAsTree(metaFormatName);

		IIOMetadataNode gceNode = getNode(root, "GraphicControlExtension");
		gceNode.setAttribute("disposalMethod", "none");
		gceNode.setAttribute("userInputFlag", "FALSE");
		gceNode.setAttribute("transparentColorFlag", "FALSE");
		gceNode.setAttribute("delayTime", Integer.toString(delay));
		gceNode.setAttribute("transparentColorIndex", "0");

		IIOMetadataNode commentsNode = getNode(root, "CommentExtensions");
		commentsNode.setAttribute("CommentExtension", "Created by: https://memorynotfound.com");

		IIOMetadataNode appExtensionsNode = getNode(root, "ApplicationExtensions");
		IIOMetadataNode child = new IIOMetadataNode("ApplicationExtension");
		child.setAttribute("applicationID", "NETSCAPE");
		child.setAttribute("authenticationCode", "2.0");

		int loopContinuously = loop ? 0 : 1;
		child.setUserObject(new byte[] {0x1, (byte) (loopContinuously & 0xFF), 0});
		appExtensionsNode.appendChild(child);
		metadata.setFromTree(metaFormatName, root);

		writer.prepareWriteSequence(null);
		return metadata;
	}

	private static IIOMetadataNode getNode(IIOMetadataNode rootNode, String nodeName) {
		int nNodes = rootNode.getLength();
		for (int i = 0; i < nNodes; i++) {
			if (rootNode.item(i).getNodeName().equalsIgnoreCase(nodeName)) {
				return (IIOMetadataNode) rootNode.item(i);
			}
		}
		IIOMetadataNode node = new IIOMetadataNode(nodeName);
		rootNode.appendChild(node);
		return node;
	}

	public void writeToSequence(BufferedImage img) throws IOException {
		IIOMetadata metadata = this.metadata != null
				  ? this.metadata
				  : (this.metadata = initMetaData(img));
		writer.writeToSequence(new IIOImage(img, null, metadata), params);
	}

	@Override
	public void close() throws IOException {
		if (metadata != null) {
			writer.endWriteSequence();
		}
		writer.reset();
	}
}
