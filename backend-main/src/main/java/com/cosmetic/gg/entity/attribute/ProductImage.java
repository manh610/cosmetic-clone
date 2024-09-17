package com.cosmetic.gg.entity.attribute;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Lob;
import javax.persistence.Table;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "product_image")
@Entity
@Getter
@Setter
public class ProductImage extends EntityBase{
	
	@Column(name = "data")
	@Lob
	private byte[] data;

	@Column(name = "type")
    private String type;

    @Column(name = "size")
    private long size;

    @Column(name = "uploaded_by")
    private String uploadedBy;
    
    @Column(name = "uploaded_at")
    private LocalDateTime uploadedAt;
    
    @Column(name = "product_id")
    private String productId;
    
    @Column(name = "review_id")
    private String reviewId;
}
